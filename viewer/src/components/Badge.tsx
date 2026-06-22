type BadgeVariant =
  | 'green'
  | 'blue'
  | 'purple'
  | 'violet'
  | 'orange'
  | 'gray'
  | 'red'
  | 'cyan'
  | 'yellow'
  | 'pink'
  | 'indigo'
  | 'teal';

interface BadgeProps {
  variant: BadgeVariant;
  children: React.ReactNode;
}

// Badges are quiet outline chips: a shared neutral shell carries the shape, and
// only the text color signals meaning. This keeps a metadata row from turning
// into a strip of loud, competing pills.
const variantText: Record<BadgeVariant, string> = {
  green: 'text-kind-enum',
  blue: 'text-kind-module',
  purple: 'text-kind-type',
  violet: 'text-kind-type',
  orange: 'text-kind-bitflags',
  gray: 'text-fg-muted',
  red: 'text-accent',
  cyan: 'text-kind-alias',
  yellow: 'text-kind-extern',
  pink: 'text-kind-type',
  indigo: 'text-kind-function',
  teal: 'text-kind-alias',
};

const shell = 'inline-flex items-center rounded-md border border-edge bg-surface font-medium';

export function Badge({ variant, children }: BadgeProps) {
  return (
    <span className={`${shell} px-2.5 py-0.5 text-xs ${variantText[variant]}`}>{children}</span>
  );
}

export function SmallBadge({
  variant,
  children,
  className = '',
}: BadgeProps & { className?: string }) {
  return (
    <span className={`${shell} px-1.5 py-0.5 text-[11px] ${variantText[variant]} ${className}`}>
      {children}
    </span>
  );
}
