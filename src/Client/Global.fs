module Global

type Page =
    | Home
    | Counter
    | About
    | ListWorkflows

let toHash page =
    match page with
    | About -> "#about"
    | Counter -> "#counter"
    | Home -> "#home"
    | ListWorkflows -> "#workflows"
