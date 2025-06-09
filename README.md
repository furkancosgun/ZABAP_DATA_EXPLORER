# ABAP Data Explorer

A powerful data viewer application for displaying, analyzing, and navigating between related ABAP tables.

## Features

- **Easy Table Viewing**: Quickly display any ABAP table with customizable row limits
- **Dynamic Filtering**: Apply WHERE conditions to filter your data
- **Smart Column Optimization**: Automatically hide empty columns for better readability
- **Technical Name Display**: Toggle between technical and descriptive field names
- **Foreign Key Navigation**: Click on relationship fields to navigate to related tables
- **Text Table Support**: Automatically detects and offers to view text tables

## Usage

1. Execute the program
2. Enter table name and optional parameters:
   - Row limit (default: 500)
   - WHERE condition (optional)
   - Display technical names (checkbox)
   - Optimize empty columns (checkbox)
3. Click execute or press F8

## Navigation Features

Click on any foreign key field to:
- View the related check table
- Choose between main table and text table (when available)
- Automatically apply relevant WHERE conditions

## Contribution Guidelines

Contributions are welcome! Please follow these guidelines:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request

## License

MIT License - Free for use and modification