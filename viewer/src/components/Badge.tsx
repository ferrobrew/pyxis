type BadgeVariant = 'green' | 'blue' | 'purple' | 'orange' | 'gray' | 'red';

interface BadgeProps {
  variant: BadgeVariant;
  children: React.ReactNode;
}

const variantClasses: Record<BadgeVariant, string> = {
  green: 'bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200',
  blue: 'bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200',
  purple: 'bg-purple-100 dark:bg-purple-900 text-purple-800 dark:text-purple-200',
  orange: 'bg-orange-100 dark:bg-orange-900 text-orange-800 dark:text-orange-200',
  gray: 'bg-gray-100 dark:bg-purple-900 text-gray-800 dark:text-purple-100',
  red: 'bg-red-100 dark:bg-red-900 text-red-800 dark:text-red-200',
};

export function Badge({ variant, children }: BadgeProps) {
  return (
    <span className={`px-3 py-1 rounded-md text-sm ${variantClasses[variant]}`}>{children}</span>
  );
}

export function SmallBadge({ variant, children }: BadgeProps) {
  return <span className={`px-2 py-1 rounded text-xs ${variantClasses[variant]}`}>{children}</span>;
}
