import { useEffect, useRef } from 'react';
import hljs from 'highlight.js/lib/core';
import 'highlight.js/styles/github.css';

// Pyxis only emits three backends (rust, cpp, json), so register just those
// languages rather than pulling in the full highlight.js bundle.
import cpp from 'highlight.js/lib/languages/cpp';
import rust from 'highlight.js/lib/languages/rust';
import json from 'highlight.js/lib/languages/json';

hljs.registerLanguage('cpp', cpp);
hljs.registerLanguage('c++', cpp);
hljs.registerLanguage('rust', rust);
hljs.registerLanguage('json', json);

interface CodeBlockProps {
  code: string;
  language?: string;
}

export function CodeBlock({ code, language }: CodeBlockProps) {
  const codeRef = useRef<HTMLElement>(null);

  useEffect(() => {
    if (codeRef.current) {
      // Clear existing highlighting
      codeRef.current.removeAttribute('data-highlighted');

      if (language) {
        try {
          hljs.highlightElement(codeRef.current);
        } catch (error) {
          // If language not supported, just show plain text
          console.warn(`Syntax highlighting failed for language: ${language}`, error);
        }
      }
    }
  }, [code, language]);

  return (
    <pre className="text-xs md:text-sm font-mono bg-inset overflow-x-auto">
      <code ref={codeRef} className={language ? `language-${language.toLowerCase()}` : ''}>
        {code}
      </code>
    </pre>
  );
}
