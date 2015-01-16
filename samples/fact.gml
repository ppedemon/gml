% Compute a factorial

{ /self /n
  n 2 lessi
  { 1 }
  { n n 1 subi self self apply muli }
  if
} /fact

12 fact fact apply
