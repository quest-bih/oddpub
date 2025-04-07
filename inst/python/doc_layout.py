
import logging
import shutil
from doclayout_yolo import YOLOv10
from huggingface_hub import hf_hub_download

import fitz  # PyMuPDF
from pathlib import Path
from typing import List, Dict, Any
import csv
from PIL import Image
import numpy as np

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

"""
PDF Layout Processing Module

This module provides functionality for analyzing and processing PDF document layouts using YOLO-based detection.
It can identify and classify different document elements such as titles, text blocks, figures, tables, and formulas.

The module uses a pre-trained YOLO model to detect and classify document elements, and provides
capabilities for handling different page layouts (single column, two columns, three columns, or irregular layouts).
"""

class PDFLayoutProcessor:
    """
    A class for processing PDF layouts and detecting document elements using YOLO.
    
    This class handles:
    - Loading and managing the YOLO model
    - Processing PDF documents to detect layout elements
    - Handling different page layouts (1-3 columns or irregular)
    - Drawing annotations on the PDF
    - Saving detection results
    
    Attributes:
        model_dir (Path): Directory for storing the YOLO model
        model_path (Path): Path to the YOLO model file
        model (YOLOv10): Loaded YOLO model instance
    """

    _model_instance = None  # Class variable to store the singleton model instance
    
    def __init__(self, model_repo: str = "juliozhao/DocLayout-YOLO-DocStructBench",
                 model_filename: str = "doclayout_yolo_docstructbench_imgsz1024.pt"):
        """
        Initialize the PDF Layout Processor.
        
        Args:
            model_repo (str): HuggingFace repository containing the YOLO model
            model_filename (str): Name of the model file to download/use
        """
        self.model_dir = Path("./models")
        self.model_path = self.model_dir / model_filename
        self.device = self._get_device()
        
        # Use the class's model instance if it exists, otherwise create it
        if PDFLayoutProcessor._model_instance is None:
            PDFLayoutProcessor._model_instance = self._load_model(model_repo, model_filename)
        self.model = PDFLayoutProcessor._model_instance

    def _get_device(self) -> str:
        """
        Determine the best available device for model inference.
        
        Returns:
            str: 'mps' for Apple Silicon, 'cuda' for NVIDIA GPU, or 'cpu' as fallback
        """
        try:
            import torch
            if torch.backends.mps.is_available():
                logger.info("Using MPS device")
                return "mps"
            elif torch.cuda.is_available():
                logger.info("Using CUDA device")
                return "cuda"
        except:
            pass
        logger.info("Using CPU device")
        return "cpu"

    def _load_model(self, model_repo: str, model_filename: str) -> YOLOv10:
        """
        Load or download the YOLO model from HuggingFace.
        
        Args:
            model_repo (str): HuggingFace repository ID
            model_filename (str): Name of the model file
            
        Returns:
            YOLOv10: Loaded YOLO model instance
        """
        self.model_dir.mkdir(parents=True, exist_ok=True)
        if not self.model_path.exists():
            logger.info(f"Downloading model from {model_repo}/{model_filename}")
            hf_hub_download(repo_id=model_repo, filename=model_filename, local_dir=self.model_dir)
        else:
            logger.info(f"Loading existing model from {self.model_path}")
        return YOLOv10(str(self.model_path), verbose=True)

    def _get_pdf_dimensions(self, pdf_path: Path) -> tuple:
        """
        Get dimensions of the first page of a PDF at 300 DPI.
        
        Args:
            pdf_path (Path): Path to the PDF file
            
        Returns:
            tuple: (width, height) in pixels at 300 DPI
        """
        with fitz.open(pdf_path) as pdf_document:
            first_page = pdf_document[0]
            width = int(first_page.rect.width * 300 / 72)  # Convert to pixels at 300 DPI
            height = int(first_page.rect.height * 300 / 72)
        logger.info(f"PDF dimensions: {width}x{height} pixels")
        return width, height

    def _process_detection_results(self, det_res, page_num: int) -> List[Dict[str, Any]]:
        """
        Process YOLO detection results into a structured format.
        
        Args:
            det_res: Raw detection results from YOLO model
            page_num (int): Current page number (1-based)
            
        Returns:
            List[Dict[str, Any]]: List of processed detections with class_id, confidence, and coordinates
        """
        detections = []
        for box in det_res[0].boxes:
            coords = box.xyxy[0].tolist()
            y0 = coords[1]  # Get y0 coordinate
            
            # Override class_id to 2 (abandon) if y0 is less than 50 or greater than 3250
            class_id = 2 if (y0 < 50.0 or y0 > 3250.0) else int(box.cls)
            
            detection = {
                "class_id": class_id,
                "confidence": float(box.conf),
                "coordinates": coords
            }
            detections.append(detection)
        logger.info(f"Page {page_num}: Found {len(detections)} detections")
        return detections

    def _cleanup_temp_dir(self, temp_dir: Path) -> None:
        """
        Safely remove temporary directory and all its contents.
        
        Args:
            temp_dir (Path): Path to temporary directory to clean up
        """
        try:
            if temp_dir.exists():
                shutil.rmtree(temp_dir)
                logger.info(f"Cleaned up temporary directory: {temp_dir}")
        except Exception as e:
            logger.warning(f"Failed to clean up temporary directory {temp_dir}: {e}")

    def _save_detection_results(self, pages_data: Dict[str, List[Dict]], output_path: Path) -> None:
        """
        Save detection results to a CSV file.
        
        Args:
            pages_data (Dict[str, List[Dict]]): Detection results organized by page
            output_path (Path): Path where CSV file will be saved
        """
        output_path = output_path.with_suffix('.csv')
        
        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            # Add order to header
            writer.writerow(['page_number', 'order', 'class_id', 'confidence', 'x0', 'y0', 'x1', 'y1'])
            
            # Write data with order
            for page_key, detections in pages_data.items():
                page_num = int(page_key.split('_')[1])
                for order, detection in enumerate(detections, start=1):
                    coords = detection['coordinates']
                    writer.writerow([
                        page_num,
                        order,
                        detection['class_id'],
                        f"{detection['confidence']:.4f}",
                        f"{coords[0]}",
                        f"{coords[1]}",
                        f"{coords[2]}",
                        f"{coords[3]}"
                    ])
        logger.info(f"Saved detection results to {output_path}")


    def _filter_overlapping_elements(self, elements: List[Dict[str, Any]], overlap_threshold: float = 0.6) -> List[Dict[str, Any]]:
        """
        Filter out overlapping elements, keeping only the larger box when there's significant overlap.
        
        Args:
            elements (List[Dict[str, Any]]): List of detected elements
            overlap_threshold (float): Minimum overlap ratio to consider boxes as overlapping
            
        Returns:
            List[Dict[str, Any]]: Filtered elements with overlapping elements removed
        """
        if not elements:
            return []
        
        # Function to calculate box area
        def box_area(box):
            return (box[2] - box[0]) * (box[3] - box[1])
        
        # Function to calculate overlap area between two boxes
        def overlap_area(box1, box2):
            # Calculate intersection coordinates
            x_left = max(box1[0], box2[0])
            y_top = max(box1[1], box2[1])
            x_right = min(box1[2], box2[2])
            y_bottom = min(box1[3], box2[3])
            
            # Check if there is an overlap
            if x_right <= x_left or y_bottom <= y_top:
                return 0.0
            
            return (x_right - x_left) * (y_bottom - y_top)
        
        # Sort elements by area (largest first)
        elements = sorted(elements, key=lambda x: box_area(x['coordinates']), reverse=True)
        
        result = []
        for elem in elements:
            coords = elem['coordinates']
            elem_area = box_area(coords)
            should_keep = True
            
            # Check against all elements we've already decided to keep
            for kept_elem in result:
                kept_coords = kept_elem['coordinates']
                
                # Calculate overlap
                overlap = overlap_area(coords, kept_coords)
                
                # If overlap is significant relative to the current element's area
                if overlap / elem_area > overlap_threshold:
                    should_keep = False
                    break
            
            if should_keep:
                result.append(elem)
        
        return result

    def _reorder_detections(self, elements: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Reorder detected elements based on page layout analysis.
        
        Analyzes the layout to determine column structure and orders elements
        according to reading order (top-to-bottom, left-to-right).
        
        Args:
            elements (List[Dict[str, Any]]): List of detected elements
            
        Returns:
            List[Dict[str, Any]]: Reordered elements following natural reading order
        """
        if not elements:
            return elements
        
        # First, filter out overlapping elements
        elements = self._filter_overlapping_elements(elements)
        
        # Convert elements to numpy arrays for analysis
        boxes = np.array([elem['coordinates'] for elem in elements])
        
        # Calculate page dimensions
        page_width = np.max(boxes[:, 2])  # rightmost coordinate
        page_height = np.max(boxes[:, 3])  # bottommost coordinate
        
        # Detect the number of columns
        num_columns = self._detect_columns(boxes, page_width)
        
        if num_columns == 1:
            # Single column: Sort all elements top-to-bottom
            return self._sort_single_column(elements)
        elif num_columns == 2:
            # Two columns: Separate into left and right columns, then sort each
            return self._sort_two_columns(elements, page_width / 2)
        elif num_columns == 3:
            # Three columns: Separate into three columns, then sort each
            return self._sort_three_columns(elements, page_width / 3, 2 * page_width / 3)
        else:
            # Irregular layout: Process section by section
            return self._process_irregular_layout(elements, page_height)

    def _detect_columns(self, boxes: np.ndarray, page_width: float) -> int:
        """
        Detect the number of columns in the layout using histogram analysis.
        
        Args:
            boxes (np.ndarray): Array of bounding boxes
            page_width (float): Width of the page
            
        Returns:
            int: Number of columns detected (1, 2, or 3)
        """
        x_coords = boxes[:, 0]  # Extract x-coordinates of bounding boxes
        histogram, bin_edges = np.histogram(x_coords, bins=20, range=(0, page_width))
        
        # Identify significant peaks in the histogram (indicating columns)
        peaks = [i for i, count in enumerate(histogram) if count > len(boxes) * 0.1]
        
        # Determine the number of columns based on the number of peaks
        if len(peaks) <= 1:
            return 1
        elif len(peaks) == 2:
            return 2
        elif len(peaks) >= 3:
            return 3
        else:
            return 1  # Default to single column if unsure

    def _sort_single_column(self, elements: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Sort elements in a single column layout from top to bottom.
        
        Args:
            elements (List[Dict[str, Any]]): List of elements to sort
            
        Returns:
            List[Dict[str, Any]]: Sorted elements
        """
        return sorted(elements, key=lambda elem: elem['coordinates'][1])

    def _sort_two_columns(self, elements: List[Dict[str, Any]], middle_line: float) -> List[Dict[str, Any]]:
        """
        Sort elements in a two-column layout.
        
        Args:
            elements (List[Dict[str, Any]]): List of elements to sort
            middle_line (float): X-coordinate separating the two columns
            
        Returns:
            List[Dict[str, Any]]: Sorted elements (left column followed by right column)
        """
        left_column = []
        right_column = []
        
        for elem in elements:
            x0 = elem['coordinates'][0]
            if x0 < middle_line:
                left_column.append(elem)
            else:
                right_column.append(elem)
        
        # Sort each column by vertical position
        left_column.sort(key=lambda elem: elem['coordinates'][1])
        right_column.sort(key=lambda elem: elem['coordinates'][1])
        
        # Combine columns: left first, then right
        return left_column + right_column

    def _sort_three_columns(self, elements: List[Dict[str, Any]], left_line: float, right_line: float) -> List[Dict[str, Any]]:
        """
        Sort elements in a three-column layout.
        
        Args:
            elements (List[Dict[str, Any]]): List of elements to sort
            left_line (float): X-coordinate separating left and middle columns
            right_line (float): X-coordinate separating middle and right columns
            
        Returns:
            List[Dict[str, Any]]: Sorted elements (left, middle, then right columns)
        """
        left_column = []
        middle_column = []
        right_column = []
        
        for elem in elements:
            x0 = elem['coordinates'][0]
            if x0 < left_line:
                left_column.append(elem)
            elif x0 < right_line:
                middle_column.append(elem)
            else:
                right_column.append(elem)
        
        # Sort each column by vertical position
        left_column.sort(key=lambda elem: elem['coordinates'][1])
        middle_column.sort(key=lambda elem: elem['coordinates'][1])
        right_column.sort(key=lambda elem: elem['coordinates'][1])
        
        # Combine columns: left first, then middle, then right
        return left_column + middle_column + right_column

    def _process_irregular_layout(self, elements: List[Dict[str, Any]], page_height: float) -> List[Dict[str, Any]]:
        """
        Process elements in an irregular layout by analyzing sections.
        
        Args:
            elements (List[Dict[str, Any]]): List of elements to process
            page_height (float): Height of the page
            
        Returns:
            List[Dict[str, Any]]: Processed and ordered elements
        """
        sections = []
        current_section = []
        previous_y = -np.inf
        
        for elem in sorted(elements, key=lambda elem: elem['coordinates'][1]):
            y0 = elem['coordinates'][1]
            
            # Check if there's a significant gap between sections
            if y0 - previous_y > page_height * 0.1:  # Define a threshold for section separation
                if current_section:
                    sections.append(current_section)
                    current_section = []
            
            current_section.append(elem)
            previous_y = y0
        
        if current_section:
            sections.append(current_section)
        
        # Sort each section individually
        reordered_elements = []
        for section in sections:
            reordered_elements.extend(self._sort_single_column(section))
        
        return reordered_elements
    
    def process_pdf(self, pdf_path: str, pdfs_dir: str=None) -> tuple:
        """
        Process a PDF file to detect and analyze its layout.
        
        This method:
        1. Converts PDF pages to images
        2. Runs YOLO detection on each page
        3. Analyzes and orders detected elements
        4. Draws annotations on the PDF
        5. Saves detection results
        
        Args:
            pdf_path (str): Path to the input PDF file
            pdfs_dir (str, optional): Directory to store output files. 
                                      If None, defaults to "pdfs"
            
        Returns:
            tuple: (output_pdf_path, results_path)
                - output_pdf_path: Path to annotated PDF
                - results_path: Path to CSV file with detection results
        """
        pdf_path = Path(pdf_path)
        pdf_name = pdf_path.stem
        
        # Create pdfs directory and PDF-specific subdirectory
        if pdfs_dir is None:
            pdfs_dir = Path("pdfs")
        else:
            pdfs_dir = Path(pdfs_dir)
        pdf_output_dir = pdfs_dir / pdf_name
        pdfs_dir.mkdir(exist_ok=True)
        pdf_output_dir.mkdir(exist_ok=True)
        
        temp_dir = pdf_output_dir / f"{pdf_name}_temp"
        
        # Define category names and colors (RGB format)
        category_names = {
            0: 'title', 1: 'plain text', 2: 'abandon', 3: 'figure',
            4: 'figure_caption', 5: 'table', 6: 'table_caption',
            7: 'table_footnote', 8: 'isolate_formula', 9: 'formula_caption'
        }
        category_colors = {
            0: (1, 0, 0),      # Red for title
            1: (0, 0.5, 0),    # Green for plain text
            2: (0.5, 0.5, 0.5),# Gray for abandon
            3: (0, 0, 1),      # Blue for figure
            4: (1, 0.5, 0),    # Orange for figure caption
            5: (0.5, 0, 0.5),  # Purple for table
            6: (0, 0.5, 0.5),  # Teal for table caption
            7: (1, 0, 1),      # Magenta for table footnote
            8: (0.7, 0.3, 0),  # Brown for isolate formula
            9: (0, 0.7, 0.7)   # Cyan for formula caption
        }

        try:
            # Create temporary directory
            temp_dir.mkdir(exist_ok=True)
            width, height = self._get_pdf_dimensions(pdf_path)
            pages_data = {}

            # Open the PDF for modification
            pdf_document = fitz.open(pdf_path)
            output_pdf_path = pdf_output_dir / f"{pdf_name}_processed.pdf"
            
            for page_num in range(len(pdf_document)):
                # Convert PDF page to image for YOLO detection
                page = pdf_document[page_num]
                pix = page.get_pixmap(matrix=fitz.Matrix(300/72, 300/72))
                temp_image_path = temp_dir / f'temp_page_{page_num+1}.png'
                pix.save(str(temp_image_path))

                # Perform prediction
                det_res = self.model.predict(
                    str(temp_image_path),
                    imgsz=1024,
                    conf=0.2,
                    device=self.device
                )

                # Process detections
                page_detections = self._process_detection_results(det_res, page_num+1)
                ordered_detections = self._reorder_detections(page_detections)
                pages_data[f"page_{page_num+1}"] = ordered_detections

                # Draw annotations on PDF
                for idx, detection in enumerate(ordered_detections):
                    class_id = detection['class_id']
                    conf = detection['confidence']
                    coords = detection['coordinates']
                    # print(coords)
                    
                    # Convert coordinates from 300 DPI to PDF coordinates (72 DPI)
                    scale = 72 / 300
                    rect = fitz.Rect(
                        (coords[0] - 7) * scale,  # Left with padding
                        (coords[1] - 1) * scale,  # Top with padding
                        (coords[2] + 2) * scale,  # Right with padding
                        (coords[3] + 1) * scale   # Bottom with padding
                    )
                    
                    color = category_colors[class_id]
                    
                    # Draw rectangle
                    page.draw_rect(rect, color=color, width=1)
                    
                    # Add text annotation with category name and confidence
                    text = f"{idx+1}. {category_names[class_id]} ({conf:.2%})"
                    text_point = fitz.Point(rect.x0, rect.y0)
                    # Create background rectangle for text
                    text_width = fitz.get_text_length(text, fontsize=3)
                    text_height = 2  # Approximate height for the text
                    text_rect = fitz.Rect(
                        text_point.x,
                        text_point.y - text_height,  # Position background above text
                        text_point.x + text_width,
                        text_point.y  # Background ends at text start point
                    )
                    page.draw_rect(text_rect, color=color, fill=color)
                    
                    # Add text in white color over the background
                    page.insert_text(
                        text_point,
                        text,
                        fontsize=3,
                        color=(1, 1, 1)  # White text
                    )

                # Clean up temporary image
                temp_image_path.unlink()

            # Save the modified PDF
            pdf_document.save(str(output_pdf_path))
            pdf_document.close()

            # Save detection results to JSON
            results_path = pdf_output_dir / f"{pdf_name}_detections.csv"
            self._save_detection_results(pages_data, results_path)

            return str(output_pdf_path), str(results_path)

        finally:
            # Clean up temporary directory
            self._cleanup_temp_dir(temp_dir)

 

# # Usage example
# if __name__ == "__main__":
#     processor = PDFLayoutProcessor()
#     try:
#         output_pdf, output_json = processor.process_pdf("10.1002+nbm.1786.pdf")
#         logger.info("Processing complete")
#     except Exception as e:
#         logger.error(f"An error occurred during processing: {e}")