import os
import csv
import fitz  # PyMuPDF
from typing import List, Dict
import logging
from pathlib import Path
from PIL import Image
from doc_layout import PDFLayoutProcessor
from utils import *
from sections import METHODS_TERMS,DATA_AVAILABILITY,DISCUSSION_TERMS,RESULTS_TERMS
from  extractor_helper import extract_section, remove_references_section
import re 
# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

class PDFProcessor:
    """
    A class for processing PDF documents with various extraction and modification capabilities.
    
    This class provides functionality to:
    - Remove irrelevant boxes from PDFs
    - Extract figures, tables, and text
    - Convert PDF content to markdown format
    - Process and validate detection results
    
    Attributes:
        pdf_path (str): Path to the input PDF file
        pdf_name (str): Name of the PDF file without extension
        pdf_dir (Path): Directory for storing processed files
        results_csv (str): Path to the CSV file containing detection results
        output_pdf (str): Path to the output PDF file
        page_width (float): Width of the PDF page in points
        page_height (float): Height of the PDF page in points
    """
    
    def __init__(self, pdf_path: str, results_csv: str = None, output_pdf: str = None, output_dir: str = None):
        """
        Initialize the PDF processor with file paths.
        
        Args:
            pdf_path (str): Path to the input PDF file
            results_csv (str, optional): Path to the CSV file containing detection results. 
                                         If None, defaults to "{pdf_name}_results.csv"
            output_pdf (str, optional): Path where the processed PDF will be saved.
                                        If None, defaults to "{pdf_name}_processed.pdf"
            output_dir (str, optional): Directory where all output files will be stored.
                                        If None, defaults to 'pdfs/{pdf_name}'
        """
        self.pdf_path = pdf_path
        self.pdf_name = Path(pdf_path).stem
        
        # Set default output directory if not provided
        if output_dir is None:
            output_dir = 'pdfs'
        # Create output directory if it doesn't exist
        output_path = Path(output_dir)
        if not output_path.exists():
            output_path.mkdir(parents=True, exist_ok=True)
            
        self.output_dir = output_path
        
        # Create PDF-specific directory
        self.pdf_dir = output_path / self.pdf_name
        self.pdf_dir.mkdir(parents=True, exist_ok=True)
        
        # Set default values if not provided
        if results_csv is None:
            results_csv = f"{self.pdf_name}_detections.csv"
        if output_pdf is None:
            output_pdf = f"{self.pdf_name}_processed.pdf"
        
        # Update paths to use pdf directory
        self.results_csv = str(self.pdf_dir / Path(results_csv).name)
        self.output_pdf = str(self.pdf_dir / Path(output_pdf).name)
        
        # Get PDF dimensions
        doc = fitz.open(pdf_path)
        self.page_width = doc[0].rect.width
        self.page_height = doc[0].rect.height
        doc.close()
        
    def validate_inputs(self) -> bool:
        """
        Validate that input files and directories exist.
        
        Returns:
            bool: True if all required files exist, False otherwise
        """
        if not os.path.exists(self.pdf_path):
            logging.error(f"PDF file not found: {self.pdf_path}")
            return False
        if not os.path.exists(self.results_csv):
            logging.error(f"Results CSV file not found: {self.results_csv}")
            return False
        return True
    
    def scale_coordinates(self, coords: List[float], image_width: int, image_height: int) -> fitz.Rect:
        """
        Scale coordinates from image space (300 DPI) to PDF space (72 DPI).
        
        Args:
            coords (List[float]): List of coordinates [x0, y0, x1, y1] in image space
            image_width (int): Width of the image in pixels
            image_height (int): Height of the image in pixels
            
        Returns:
            fitz.Rect: Scaled coordinates in PDF space
        """
        # Convert from image coordinates to PDF points
        x0 = coords[0] * self.page_width / image_width
        y0 = coords[1] * self.page_height / image_height
        x1 = coords[2] * self.page_width / image_width
        y1 = coords[3] * self.page_height / image_height
        return fitz.Rect(x0, y0, x1, y1)
    
    def process_detections(self, detection_row: Dict[str, str], page_num: int) -> List[fitz.Rect]:
        """
        Process detections for a single page and return list of rectangles.
        
        Args:
            detection_row (Dict[str, str]): Dictionary containing detection information
            page_num (int): Page number being processed (0-based)
            
        Returns:
            List[fitz.Rect]: List of rectangles to be redacted
        """
        redactions = []
        try:
            # Calculate image dimensions for this page (300 DPI)
            image_width = int(self.page_width * 300 / 72)
            image_height = int(self.page_height * 300 / 72)
            
            class_id = int(detection_row.get("class_id", -1))
            confidence = float(detection_row.get("confidence", 0))
            
            # Extract coordinates directly from the CSV columns
            coords = [
                float(detection_row.get("x0", 0)),
                float(detection_row.get("y0", 0)),
                float(detection_row.get("x1", 0)),
                float(detection_row.get("y1", 0))
            ]
            
            if class_id == 2 and confidence >= 0.2:  # Only process class_id 2 with confidence >= 0.2
                rect = self.scale_coordinates(coords, image_width, image_height)
                redactions.append(rect)
                
        except Exception as e:
            logging.error(f"Error processing detections for page {page_num + 1}: {e}")
        return redactions
    
    def remove_irrelevant_boxes(self) -> bool:
        """
        Process the PDF and remove irrelevant boxes based on detection results.
        
        If the detection results don't exist, it will process the PDF using PDFLayoutProcessor
        to generate them first.
        
        Returns:
            bool: True if processing was successful, False otherwise
        """
        if not self.validate_inputs():
            # Process the PDF using PDFLayoutProcessor if validation fails
            try:
                processor = PDFLayoutProcessor()
                _, output_csv = processor.process_pdf(self.pdf_path,self.output_dir)
                logging.info(f"PDF processed using PDFLayoutProcessor. Results saved to {output_csv}")
                self.results_csv = output_csv  # Update the results CSV path
                return self.remove_irrelevant_boxes()  # Retry with the new CSV
            except Exception as e:
                logging.error(f"Error processing PDF with PDFLayoutProcessor: {e}")
                return False
        
        try:
            doc = fitz.open(self.pdf_path)
            modifications_made = False
            
            # Create a dictionary to store detections by page
            page_detections = {}
            
            # Read CSV file
            with open(self.results_csv, "r") as f:
                csv_reader = csv.DictReader(f)
                for row in csv_reader:
                    # Fix: Use correct column name from CSV
                    page_num = int(row.get("page_number", 0)) - 1
                    if page_num not in page_detections:
                        page_detections[page_num] = []
                    page_detections[page_num].append(row)
                    
            # Process each page
            for page_num in range(len(doc)):
                if page_num in page_detections:
                    page = doc[page_num]
                    detections = page_detections[page_num]
                    
                    all_redactions = []
                    for detection in detections:
                        redactions = self.process_detections(detection, page_num)
                        all_redactions.extend(redactions)
                    
                    if all_redactions:
                        modifications_made = True
                        for rect in all_redactions:
                            page.add_redact_annot(rect, fill=(1, 1, 1))  # White fill
                        page.apply_redactions()
                        logging.info(f"Applied {len(all_redactions)} redactions to page {page_num + 1}")
                else:
                    logging.warning(f"No detection results found for page {page_num + 1}")
            
            if modifications_made:
                doc.save(self.output_pdf)
                logging.info(f"Saved modified PDF to {self.output_pdf}")
            else:
                logging.warning("No modifications were made to the PDF")
            
            doc.close()
            return True
        except Exception as e:
            logging.error(f"Error processing PDF: {e}")
            return False

    def extract_figures(self) -> List[str]:
        """
        Extract figures from PDF using detection results.
        
        This method extracts figures detected in the PDF and saves them as individual image files.
        If detection results don't exist, it will first process the PDF to generate them.
        
        Returns:
            List[str]: List of paths to extracted figure files
        """
        if not Path(self.results_csv).exists():
            logging.info(f"Detection results not found. Processing PDF: {self.pdf_path}")
            processor = PDFLayoutProcessor()
            _, output_csv = processor.process_pdf(str(self.pdf_path),str(self.output_dir))
            self.results_csv = output_csv

        output_dir = self.pdf_dir / 'figures' 
        output_dir.mkdir(parents=True, exist_ok=True)
        extracted_figures = []

        try:
            # Read detection results
            page_figures = {}
            with open(self.results_csv, 'r') as f:
                csv_reader = csv.DictReader(f)
                for row in csv_reader:
                    if int(row['class_id']) == 3:  # Figure class ID
                        page_num = int(row['page_number']) - 1
                        if page_num not in page_figures:
                            page_figures[page_num] = []
                        page_figures[page_num].append({
                            'x0': float(row['x0']),
                            'y0': float(row['y0']),
                            'x1': float(row['x1']),
                            'y1': float(row['y1'])
                        })

            # Extract figures from PDF
            doc = fitz.open(self.pdf_path)
            for page_num, figures in page_figures.items():
                page = doc[page_num]
                pix = page.get_pixmap(matrix=fitz.Matrix(300/72, 300/72))
                img = Image.frombytes("RGB", [pix.width, pix.height], pix.samples)

                for fig_idx, coords in enumerate(figures):
                    # Convert coordinates from 300 DPI to image coordinates
                    x0 = int(coords['x0'])
                    y0 = int(coords['y0'])
                    x1 = int(coords['x1'])
                    y1 = int(coords['y1'])

                    # Crop and save figure
                    figure = img.crop((x0, y0, x1, y1))
                    figure_name = f"{self.pdf_name}_page{page_num + 1}_figure{fig_idx + 1}.png"
                    figure_path = output_dir / figure_name
                    figure.save(str(figure_path))
                    extracted_figures.append(str(figure_path))
                    logging.info(f"Extracted figure: {figure_path}")

            return extracted_figures

        except Exception as e:
            logging.error(f"Error extracting figures from PDF: {e}")
            return []

    def extract_tables(self) -> List[str]:
        """
        Extract tables from PDF using detection results.
        
        This method:
        1. Checks if detection results exist, processes the PDF if not
        2. Reads table coordinates from detection results
        3. Extracts tables from PDF pages
        4. Saves tables as image files
        
        Returns:
            List[str]: List of paths to extracted table files
        """
        if not Path(self.results_csv).exists():
            logging.info(f"Detection results not found. Processing PDF: {self.pdf_path}")
            processor = PDFLayoutProcessor()
            _, output_csv = processor.process_pdf(str(self.pdf_path),str(self.output_dir))
            self.results_csv = output_csv

        output_dir = self.pdf_dir / 'tables'
        output_dir.mkdir(parents=True, exist_ok=True)
        extracted_tables = []

        try:
            # Read detection results
            page_tables = {}
            with open(self.results_csv, 'r') as f:
                csv_reader = csv.DictReader(f)
                for row in csv_reader:
                    if int(row['class_id']) == 5:  # Table class ID
                        page_num = int(row['page_number']) - 1
                        if page_num not in page_tables:
                            page_tables[page_num] = []
                        page_tables[page_num].append({
                            'x0': float(row['x0']),
                            'y0': float(row['y0']),
                            'x1': float(row['x1']),
                            'y1': float(row['y1'])
                        })

            # Extract tables from PDF
            doc = fitz.open(self.pdf_path)
            for page_num, tables in page_tables.items():
                page = doc[page_num]
                pix = page.get_pixmap(matrix=fitz.Matrix(300/72, 300/72))
                img = Image.frombytes("RGB", [pix.width, pix.height], pix.samples)

                for table_idx, coords in enumerate(tables):
                    # Convert coordinates from 300 DPI to image coordinates
                    x0 = int(coords['x0'])
                    y0 = int(coords['y0'])
                    x1 = int(coords['x1'])
                    y1 = int(coords['y1'])

                    # Crop and save table
                    table = img.crop((x0, y0, x1, y1))
                    table_name = f"{self.pdf_name}_page{page_num + 1}_table{table_idx + 1}.png"
                    table_path = output_dir / table_name
                    table.save(str(table_path))
                    extracted_tables.append(str(table_path))
                    logging.info(f"Extracted table: {table_path}")

            return extracted_tables

        except Exception as e:
            logging.error(f"Error extracting tables from PDF: {e}")
            return []

    def extract_text(self) -> str:
        """
        Extract text from PDF using detection results and specified logic.
        
        This method extracts text from title and plain text regions (class_id 0 and 1)
        while maintaining proper spacing and formatting between text blocks.
        
        Returns:
            str: Extracted text content
        """
        if not Path(self.results_csv).exists():
            logging.info(f"Detection results not found. Processing PDF: {self.pdf_path}")
            processor = PDFLayoutProcessor()
            _, output_csv = processor.process_pdf(str(self.pdf_path),str(self.output_dir))
            self.results_csv = output_csv

        output_txt = self.pdf_dir / f'{self.pdf_name}.txt'
        
        try:
            doc = fitz.open(self.pdf_path)
            extracted_text = []
            
            # Get PDF dimensions for scaling
            page = doc[0]
            pdf_width = page.rect.width
            pdf_height = page.rect.height
            
            # Calculate scaling factors (300 DPI to 72 DPI)
            scale_x = pdf_width / (pdf_width * 300 / 72)
            scale_y = pdf_height / (pdf_height * 300 / 72)
            
            # Read CSV file and store rows (only class 0 and 1)
            with open(self.results_csv, 'r') as f:
                csv_reader = csv.DictReader(f)
                rows = [row for row in csv_reader if int(row['class_id']) in [0, 1] and float(row['confidence']) > 0.2]
            
            # Process each row
            for i in range(len(rows)):
                current_row = rows[i]
                current_page = doc[int(current_row['page_number']) - 1]
                
                # Scale coordinates from 300 DPI to PDF space (72 DPI)
                current_rect = fitz.Rect(
                    float(current_row['x0']) * scale_x,
                    float(current_row['y0']) * scale_y,
                    float(current_row['x1']) * scale_x,
                    float(current_row['y1']) * scale_y
                )
                
                current_text = current_page.get_text("text", clip=current_rect)
                current_links = extract_links(current_page.get_links())
                current_text = process_page_text(current_text, current_links)
                current_text = remove_unicode(current_text)  # Remove Unicode characters
                
                if not current_text:  # Skip empty text blocks
                    continue
                
                # If this is not the last row, check the next row
                if i < len(rows) - 1:
                    next_row = rows[i + 1]
                    next_page = doc[int(next_row['page_number']) - 1]
                    
                    # Scale coordinates for next rectangle
                    next_rect = fitz.Rect(
                        float(next_row['x0']) * scale_x,
                        float(next_row['y0']) * scale_y,
                        float(next_row['x1']) * scale_x,
                        float(next_row['y1']) * scale_y
                    )
                    
                    next_text = next_page.get_text("text", clip=next_rect)
                    next_links = extract_links(next_page.get_links())
                    next_text = process_page_text(next_text, next_links)
                    next_text = remove_unicode(next_text)  # Remove Unicode characters
                    
                    # Check if current and next elements are from the same class
                    if current_row['class_id'] == next_row['class_id']:
                        # If next text starts with lowercase, join with space
                        if next_text and next_text[0].islower():
                            extracted_text.append(current_text + "")
                        else:
                            extracted_text.append(current_text + "\n")
                    else:
                        # Different classes, join with double newline
                        extracted_text.append(current_text + "\n\n")
                else:
                    # Last row, just append the text
                    extracted_text.append(current_text)          

            doc.close()
            
            # Join all text and save to file
            final_text = "".join(extracted_text)
            with open(output_txt, 'w', encoding='utf-8') as f:
                f.write(final_text)
            
            logging.info(f"Extracted text saved to: {output_txt}")
            return final_text

        except Exception as e:
            logging.error(f"Error extracting text from PDF: {e}")
            return ""

    def extract_markdown(self) -> str:
        """
        Extract content from PDF and convert to markdown format.
        
        This method processes different types of content (text, figures, tables, formulas)
        and converts them to appropriate markdown syntax. Images are saved separately
        and referenced in the markdown.
        
        Returns:
            str: Generated markdown content
        """
        if not Path(self.results_csv).exists():
            logging.info(f"Detection results not found. Processing PDF: {self.pdf_path}")
            processor = PDFLayoutProcessor()
            _, output_csv = processor.process_pdf(str(self.pdf_path),str(self.output_dir))
            self.results_csv = output_csv

        output_md = self.pdf_dir / f'{self.pdf_name}.md'
        images_dir = self.pdf_dir / 'md_images'
        images_dir.mkdir(parents=True, exist_ok=True)

        try:
            doc = fitz.open(self.pdf_path)
            markdown_content = []
            
            # Get PDF dimensions for scaling
            page = doc[0]
            pdf_width = page.rect.width
            pdf_height = page.rect.height
            
            # Calculate scaling factors (300 DPI to 72 DPI)
            scale_x = pdf_width / (pdf_width * 300 / 72)
            scale_y = pdf_height / (pdf_height * 300 / 72)
            
            # Read CSV file and store rows (excluding class 2)
            with open(self.results_csv, 'r') as f:
                csv_reader = csv.DictReader(f)
                # Filter out class 2 entries
                rows = [row for row in csv_reader if int(row['class_id']) != 2]
            
            # Process each row
            for i in range(len(rows)):
                current_row = rows[i]
                class_id = int(current_row['class_id'])
                current_page = doc[int(current_row['page_number']) - 1]
                
                # Scale coordinates
                current_rect = fitz.Rect(
                    float(current_row['x0']) * scale_x,
                    float(current_row['y0']) * scale_y,
                    float(current_row['x1']) * scale_x,
                    float(current_row['y1']) * scale_y
                )
                
                # Handle different content types
                if class_id == 0:  # Title
                    text = current_page.get_text("text", clip=current_rect).encode('utf-8').decode('utf-8')
                    text = " ".join(text.split("\n"))
                    text = remove_unicode(text)  # Remove Unicode characters
                    markdown_content.append(f"# {text}\n\n")
                    
                elif class_id == 1:  # Plain text
                    text = current_page.get_text("text", clip=current_rect).encode('utf-8').decode('utf-8')
                    text = " ".join(text.split("\n"))
                    text = remove_unicode(text)  # Remove Unicode characters
                    
                    if i < len(rows) - 1:
                        next_row = rows[i + 1]
                        next_page = doc[int(next_row['page_number']) - 1]
                        next_rect = fitz.Rect(
                            float(next_row['x0']) * scale_x,
                            float(next_row['y0']) * scale_y,
                            float(next_row['x1']) * scale_x,
                            float(next_row['y1']) * scale_y
                        )
                        next_text = next_page.get_text("text", clip=next_rect).encode('utf-8').decode('utf-8')
                        next_text = " ".join(next_text.split("\n"))
                        next_text = remove_unicode(next_text)  # Remove Unicode characters
                        
                        if int(next_row['class_id']) == 1 and next_text and next_text[0].islower():
                            markdown_content.append(f"{text} ")
                        else:
                            markdown_content.append(f"{text}\n\n")
                    else:
                        markdown_content.append(f"{text}\n\n")
                        
                elif class_id == 3:  # Figure
                    # Extract and save figure
                    pix = current_page.get_pixmap(clip=current_rect)  
                    img_path = images_dir / f"figure_{i+1}.png"
                    pix.save(str(img_path))
                    markdown_content.append(f"![Figure]({img_path.relative_to(self.pdf_dir)})\n\n")
                    
                elif class_id == 4:  # Figure caption
                    text = current_page.get_text("text", clip=current_rect).encode('utf-8').decode('utf-8')
                    text = " ".join(text.split("\n"))
                    text = remove_unicode(text)  # Remove Unicode characters
                    markdown_content.append(f"__*{text}__*\n\n")
                    
                elif class_id == 5:  # Table
                    # Extract and save table
                    pix = current_page.get_pixmap(clip=current_rect)
                    img_path = images_dir / f"table_{i+1}.png"
                    pix.save(str(img_path))
                    markdown_content.append(f"![Table]({img_path.relative_to(self.pdf_dir)})\n\n")
                    
                elif class_id == 6:  # Table caption
                    text = current_page.get_text("text", clip=current_rect).encode('utf-8').decode('utf-8')
                    text = " ".join(text.split("\n"))
                    text = remove_unicode(text)  # Remove Unicode characters
                    markdown_content.append(f"__*{text}__*\n\n")
                    
                elif class_id == 7:  # Table footnote
                    text = current_page.get_text("text", clip=current_rect).encode('utf-8').decode('utf-8')
                    text = " ".join(text.split("\n"))
                    text = remove_unicode(text)  # Remove Unicode characters
                    markdown_content.append(f"__*{text}__*\n\n")
                    
                elif class_id in [8, 9]:  # Formula and formula caption
                    text = current_page.get_text("text", clip=current_rect).encode('utf-8').decode('utf-8')
                    text = " ".join(text.split("\n"))
                    text = remove_unicode(text)  # Remove Unicode characters
                    markdown_content.append(f"```math\n{text}\n```\n\n")

            doc.close()
            
            # Join all content and save to file
            final_markdown = "".join(markdown_content)
            with open(output_md, 'w', encoding='utf-8') as f:
                f.write(final_markdown)
            
            logging.info(f"Markdown content saved to: {output_md}")
            return final_markdown

        except Exception as e:
            logging.error(f"Error extracting markdown from PDF: {e}")
            return ""   
    
    def extract_sections(self, section_type=None):
        """
        Extracts specified sections from the given PDF.
        :param section_type: Type of section to extract. If None, extracts all sections.
        :param extract_all: If True, extracts all sections. If False, extracts only the specified section.
        
        :return: Dictionary of extracted sections.
        """
        pdf_txt = self.pdf_dir / f'{self.pdf_name}.txt'
        if not Path(pdf_txt).exists():
            logging.info(f"Extracting text from PDF: {self.pdf_path}")
            self.extract_text()
        with open(pdf_txt, 'r') as f:
            text = f.read()

        text_with_no_ref = remove_references_section(text)
       
        # Define terms based on the section type
        section_terms = {
            "methods": METHODS_TERMS,
            "discussion": DISCUSSION_TERMS,
            "results": RESULTS_TERMS,
            "das": DATA_AVAILABILITY  
        }

        # If section_type is None or 'all', extract all sections
        if section_type is None or section_type.lower() == 'all' or not section_type.strip():
            extracted_sections = {}
            for section_name, terms in section_terms.items():
                section_text = extract_section(text_with_no_ref, terms)
                extracted_sections[section_name] = section_text
            return extracted_sections
        
        # Extract the specified section
        terms = section_terms.get(section_type.lower())
        if terms:
            extracted_section = extract_section(text_with_no_ref, terms)
            return extracted_section if extracted_section else ""
        return ""

# def main():
    
# #     # Initialize and run processor
#     processor = PDFProcessor(pdf_path='elife-59907-v2.pdf', output_dir='pdfs1')
#     processor.extract_markdown()

# if __name__ == '__main__':
#     main()
