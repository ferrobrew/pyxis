import { useEffect, useRef } from 'react';
import hljs from 'highlight.js/lib/core';
import 'highlight.js/styles/github.css';

// Import common languages
import cpp from 'highlight.js/lib/languages/cpp';
import rust from 'highlight.js/lib/languages/rust';
import python from 'highlight.js/lib/languages/python';
import javascript from 'highlight.js/lib/languages/javascript';
import typescript from 'highlight.js/lib/languages/typescript';
import csharp from 'highlight.js/lib/languages/csharp';

// Register languages
hljs.registerLanguage('cpp', cpp);
hljs.registerLanguage('c++', cpp);
hljs.registerLanguage('rust', rust);
hljs.registerLanguage('python', python);
hljs.registerLanguage('javascript', javascript);
hljs.registerLanguage('typescript', typescript);
hljs.registerLanguage('csharp', csharp);
hljs.registerLanguage('cs', csharp);

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
    <pre className="text-sm font-mono bg-gray-100 dark:bg-slate-800 overflow-x-auto">
      <code ref={codeRef} className={language ? `language-${language.toLowerCase()}` : ''}>
        {code}
      </code>
    </pre>
  );
}
