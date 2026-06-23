import { useEffect, useRef } from 'react';
import hljs from 'highlight.js/lib/core';

// Token colours come from the theme-aware `.hljs-*` rules in index.css (which
// flip with dark mode), so we deliberately don't import a highlight.js stylesheet
// — a bundled theme (e.g. github.css) would hard-code a light background.

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
    <pre className="text-xs md:text-sm font-mono bg-inset overflow-x-auto p-3">
      <code ref={codeRef} className={language ? `language-${language.toLowerCase()}` : ''}>
        {code}
      </code>
    </pre>
  );
}
