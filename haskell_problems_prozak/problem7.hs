data NestedList a = Elem a | List [NestedList a]

flattenHelper :: NestedList a -> [a] -> [a]
flattenHelper (Elem a) xs = a : xs
flattenHelper (List nl) xs = foldl (flip flattenHelper) xs nl

flatten :: NestedList a -> [a]
flatten nl = reverse $ flattenHelper nl []
