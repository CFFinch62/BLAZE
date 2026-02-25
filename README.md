# BLAZE - Pascal Teaching Environment

BLAZE is a comprehensive, standalone integrated development environment (IDE) specifically designed for learning and writing Pascal (FPC/GPC). It features a dedicated code editor, an integrated file browser, syntax highlighting, and a built-in terminal.

## Features

- **Code Editor**: Syntax highlighting tailored for Pascal keywords, builtins, and directives.
- **File Browser**: Easily navigate your project files utilizing FABLE-like enhancements (Home, Up, Bookmarks, double-click to navigate) to open `.pas`, `.pp`, `.lpr`, and `.inc` files.
- **Integrated Terminal**: Built-in Pyte-based terminal for compiling and execution.
- **Build System**: Clean object builds and compilation via FPC under the hood. 
- **Examples**: Comes with a suite of beginner-friendly Pascal examples (e.g. `hello_world.pas`, `loops.pas`, `records.pas`).

## Running the Application

1. Make sure Python 3 is installed.
2. Run `setup.sh` to initialize the virtual environment and install dependencies.
3. Launch the IDE by running `run.sh`.

## Building a Release

Use the provided PyInstaller script to package the application into a standalone executable:
```bash
python3 build.py
```
The executable will be located in the `dist/BLAZE/` directory.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
