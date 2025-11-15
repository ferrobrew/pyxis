# Pyxis Documentation Viewer

A beautiful rustdoc-style documentation viewer for Pyxis JSON backend output.

## Features

- 🎨 **Beautiful UI**: Rustdoc-inspired design with clean, readable layout
- 🌓 **Dark/Light Mode**: Automatic theme detection with manual toggle
- 🔍 **Search**: Fast, intelligent search across all types, enums, bitflags, and functions
- 📁 **Module Navigation**: Intuitive sidebar with collapsible module tree
- 🔗 **Smart Paths**: Displays relative paths when possible to reduce clutter
- 📊 **Rich Details**: View fields, functions, variants, flags, and more
- 🎯 **Type References**: Clickable type references for easy navigation
- 📝 **Backend Info**: Collapsible sections for backend prologues and epilogues
- 🚀 **SPA Routing**: Client-side routing for seamless navigation

## Getting Started

### Development

```bash
# Install dependencies (from root of monorepo)
npm install

# Start dev server
cd viewer
npm run dev
```

### Building

```bash
# Build for production
npm run build

# Preview production build
npm run preview
```

## Usage

1. **Load Documentation**: Click "Load Documentation" in the top-left corner
2. **Select File**: Choose a Pyxis JSON documentation file (e.g., `output.json`)
3. **Browse**: Use the sidebar to navigate through modules
4. **Search**: Use the search bar to find specific types, enums, or functions
5. **View Details**: Click on any item to see its full documentation

## Project Structure

```
viewer/
├── src/
│   ├── components/        # React components
│   │   ├── Collapsible.tsx
│   │   ├── FileUpload.tsx
│   │   ├── Header.tsx
│   │   ├── ItemView.tsx
│   │   ├── ModuleView.tsx
│   │   ├── SearchBar.tsx
│   │   ├── Sidebar.tsx
│   │   ├── TypeRef.tsx
│   │   └── WelcomePage.tsx
│   ├── contexts/          # React contexts
│   │   ├── DocumentationContext.tsx
│   │   └── ThemeContext.tsx
│   ├── utils/             # Utility functions
│   │   ├── pathUtils.ts
│   │   └── searchUtils.ts
│   ├── App.tsx            # Main app component
│   ├── main.tsx           # Entry point
│   └── index.css          # Global styles
└── package.json
```

## Type Linking

The viewer uses npm workspaces to link to the `@pyxis/types` package, which contains a symlink to the generated `json.ts` types from the Pyxis backend. This ensures type safety and automatic updates when the backend types change.

## Features in Detail

### Smart Path Resolution

The viewer intelligently displays paths:
- Primitive types (u32, bool, etc.) are shown as-is
- Types in the same module show only the name
- Types in sibling modules show the relative path
- Otherwise, the full absolute path is displayed

### Search

The search feature provides:
- Real-time results as you type
- Keyboard navigation (Arrow keys, Enter, Escape)
- Prioritized results (exact matches first)
- Support for partial matches
- Type indicators (type, enum, bitflags, function, module)

### Theme Support

The viewer automatically detects your system theme preference and applies it on load. You can toggle between light and dark modes using the button in the top-right corner. Your preference is saved to localStorage.

## Future Enhancements

- Monorepo dropdown for loading multiple documentation files
- Permalink support for sharing specific items
- Export/print documentation
- Syntax highlighting for code blocks
- Collapsible sidebar
- Custom theme colors

## License

This project is part of the Pyxis toolchain.
