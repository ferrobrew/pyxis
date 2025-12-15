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

const variantClasses: Record<BadgeVariant, string> = {
  green: 'bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200',
  blue: 'bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200',
  purple: 'bg-violet-100 dark:bg-slate-800 text-violet-800 dark:text-slate-300',
  violet: 'bg-violet-100 dark:bg-slate-800 text-violet-800 dark:text-slate-300',
  orange: 'bg-orange-100 dark:bg-orange-900 text-orange-800 dark:text-orange-200',
  gray: 'bg-gray-100 dark:bg-slate-800 text-gray-800 dark:text-slate-300',
  red: 'bg-red-100 dark:bg-red-900 text-red-800 dark:text-red-200',
  cyan: 'bg-cyan-100 dark:bg-cyan-900 text-cyan-800 dark:text-cyan-200',
  yellow: 'bg-yellow-100 dark:bg-yellow-900 text-yellow-800 dark:text-yellow-200',
  pink: 'bg-pink-100 dark:bg-pink-900 text-pink-800 dark:text-pink-200',
  indigo: 'bg-indigo-100 dark:bg-indigo-900 text-indigo-800 dark:text-indigo-200',
  teal: 'bg-teal-100 dark:bg-teal-900 text-teal-800 dark:text-teal-200',
};

export function Badge({ variant, children }: BadgeProps) {
  return (
    <span className={`px-3 py-1 rounded-md text-sm ${variantClasses[variant]}`}>{children}</span>
  );
}

export function SmallBadge({
  variant,
  children,
  className = '',
}: BadgeProps & { className?: string }) {
  return (
    <span className={`px-2 py-1 rounded text-xs ${variantClasses[variant]} ${className}`}>
      {children}
    </span>
  );
}
