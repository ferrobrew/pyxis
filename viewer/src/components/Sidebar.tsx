// The sidebar shell: the mobile toggle header plus the module tree. The tree
// rendering lives in `sidebarTree.tsx` and the icons in `sidebarIcons.tsx`, so
// this file stays under the ~400-line threshold.

import { useEffect, useRef, useState } from 'react';
import { useDocumentation } from '../contexts/DocumentationContext';
import { ModuleTree } from './sidebarTree';

type SidebarProps = {
  onClose: () => void;
};

export function Sidebar({ onClose }: SidebarProps) {
  const { documentation } = useDocumentation();
  const [sidebarWidth, setSidebarWidth] = useState(() => {
    const saved = localStorage.getItem('sidebarWidth');
    return saved ? parseInt(saved, 10) : 256; // Default 256px (w-64)
  });
  const [isResizing, setIsResizing] = useState(false);
  const [isDesktop, setIsDesktop] = useState(() => window.innerWidth >= 1024);
  const sidebarRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const handleMouseMove = (e: MouseEvent) => {
      if (!isResizing) return;
      const newWidth = e.clientX;
      const minWidth = 200;
      const maxWidth = 800;
      const clampedWidth = Math.max(minWidth, Math.min(maxWidth, newWidth));
      setSidebarWidth(clampedWidth);
    };

    const handleMouseUp = () => {
      setIsResizing(false);
    };

    if (isResizing) {
      document.addEventListener('mousemove', handleMouseMove);
      document.addEventListener('mouseup', handleMouseUp);
      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
    }

    return () => {
      document.removeEventListener('mousemove', handleMouseMove);
      document.removeEventListener('mouseup', handleMouseUp);
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    };
  }, [isResizing]);

  useEffect(() => {
    localStorage.setItem('sidebarWidth', sidebarWidth.toString());
  }, [sidebarWidth]);

  // Handle responsive width changes
  useEffect(() => {
    const handleResize = () => {
      setIsDesktop(window.innerWidth >= 1024);
    };

    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  const handleMouseDown = () => {
    setIsResizing(true);
  };

  if (!documentation) {
    return (
      <div
        className="
        relative hidden
        lg:flex
      "
        style={{ width: `${sidebarWidth}px` }}
      >
        <aside className="flex-1 bg-sidebar p-4">
          <div className="text-sm text-fg-subtle">No documentation loaded</div>
        </aside>
        <div
          onMouseDown={handleMouseDown}
          className="
            hidden w-1 cursor-col-resize bg-edge transition-colors
            hover:bg-accent
            lg:block
          "
        />
      </div>
    );
  }

  return (
    <div
      className="relative flex"
      ref={sidebarRef}
      style={{
        width: isDesktop ? `${sidebarWidth}px` : '100%',
      }}
    >
      <aside className="flex-1 overflow-y-auto bg-sidebar">
        <div
          className="p-2"
          onClick={(e) => {
            // Close sidebar on mobile when clicking nav links
            if (!isDesktop && (e.target as HTMLElement).closest('a')) {
              onClose();
            }
          }}
        >
          <nav>
            {Object.entries(documentation.modules).map(([name, module]) => (
              <ModuleTree key={name} name={name} module={module} path={name} level={0} />
            ))}
          </nav>
        </div>
      </aside>

      {/* Desktop resize handle */}
      {isDesktop && (
        <div
          onMouseDown={handleMouseDown}
          className="
            w-1 cursor-col-resize bg-edge transition-colors
            hover:bg-accent
          "
        />
      )}
    </div>
  );
}
