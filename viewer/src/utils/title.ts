import { useEffect } from 'react';

/** Base browser-tab title, shown bare on the welcome page. */
export const BASE_TITLE = 'pyxisdoc';

/**
 * Keep the browser tab title in sync with the current page: `pyxisdoc - {suffix}`
 * for an item or module path (e.g. `pyxisdoc - consts::Player`), or the bare
 * `pyxisdoc` when `suffix` is empty/nullish.
 */
export function useDocumentTitle(suffix?: string | null): void {
  useEffect(() => {
    document.title = suffix ? `${BASE_TITLE} - ${suffix}` : BASE_TITLE;
  }, [suffix]);
}
