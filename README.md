# Format Converter

## Overview

The **Document Converter** is a powerful tool written in Haskell, inspired by [Pandoc](https://pandoc.org/). It allows you to convert documents between various formats, including XML, JSON, and Markdown. This tool utilizes a syntax parser to ensure accurate and efficient conversions, making it ideal for developers and users who need to work with multiple document formats.

## Features

- **Multi-Format Conversion:** Supports conversions between XML, JSON, and Markdown.
- **Custom Parsing Library**: Implements a self-made parsing library in Haskell, providing flexibility and control over document parsing.
- **Syntax Parsing:** Leverages Haskell's type system for reliable document parsing.
- **Command-Line Interface**: User-friendly CLI with options for input files, output formats, and more.


## Installation

To install and set up the Document Converter locally, follow these steps:

1. **Clone the Repository:**
    ```bash
    git clone https://github.com/yourusername/format-converter.git
    cd document-converter
    ```

2. **Build the Project:**
    Ensure you have the Haskell toolchain installed. If not, follow the instructions on [Haskell's official site](https://www.haskell.org/downloads/).

    Then, build the project using:
    ```bash
    make
    ```

## Usage

To convert a document from one format to another, use the following command:

```bash
./format-converter -i inputfile -f outputformat [-o outputfile] [-e inputformat]
```

## Document Structure

To ensure your document to be compatible with **format-converter** it must be divided into two main parts: the **Header** and the **Content**. Below is a detailed explanation of each part and how they are represented in different formats.

### 1. Header

The header contains metadata information about the document, such as the title, author, and date. Here is how the header is represented in each format:

- **XML:**
  ```xml
  <document>
    <header title="Document Title" author="Author Name" date="2024-08-28"></header>
    <body>
      <!-- Content goes here -->
    </body>
  </document>
  ```
  - The `<header>` element includes attributes like `title`, `author`, and `date`.
  - If any of these attributes are optional and not provided, they will not appear in the XML output.

- **JSON:**
  ```json
  {
    "header": {
      "title": "Document Title",
      "author": "Author Name",
      "date": "2024-08-28"
    },
    "body": [
      // Content goes here
    ]
  }
  ```
  - The header is represented as a JSON object with keys: `title`, `author`, and `date`.
  - If an optional field (e.g., `author` or `date`) is not provided, it may be omitted or set to `null`.

- **Markdown:**
  ```
  ---
  title: Document Title
  author: Author Name
  date: 2024-08-28
  ---
  ```
  - Markdown uses front matter, a block at the top of the document delimited by `---`, to specify header information.
  - Only the provided fields will appear; any missing fields will be excluded.

### 2. Content

The content section is the main body of the document. It consists of various elements such as text, formatting, links, images, structural elements (paragraphs, sections, code blocks), and lists.

## License

This project is released under the MIT License. See the LICENSE file for more details.
