// Group a hex address into threes from the right, the way pyxis-defs sources
// are written: 0x143_C69_C40.
export function formatHexAddress(n: number): string {
  const grouped = n.toString(16).replace(/\B(?=(.{3})+(?!.))/g, '_');
  return `0x${grouped}`;
}
