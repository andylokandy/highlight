use std::fmt::{format, Debug};

use smallvec::SmallVec;

use crate::{
    syntax::{Semantic, Span},
    token::{Token, TokenKind},
};

#[derive(Debug, Clone, Copy)]
pub struct Input<'a> {
    pub tokens: &'a [Token<'a>],
    pub fast_path: bool,
}

#[derive(Debug)]
pub struct Solution<'a, T, S, E> {
    pub consumed: Vec<(Span, S)>,
    pub errors: Vec<(Span, E)>,
    pub rest: Input<'a>,
    pub value: Option<T>,
}

#[derive(Debug)]
pub struct Solutions<'a, T, S, E>
where
    T: Debug,
    S: Debug,
    E: Debug,
{
    solutions: SmallVec<[Solution<'a, T, S, E>; 1]>,
}

impl<'a, T, S, E> Solutions<'a, T, S, E>
where
    T: Debug,
    S: Debug,
    E: Debug,
{
    pub fn new() -> Self {
        Solutions {
            solutions: SmallVec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.solutions.len()
    }

    pub fn valid_len(&self) -> usize {
        self.solutions.iter().filter(|s| s.value.is_some()).count()
    }

    pub fn push(&mut self, solution: Solution<'a, T, S, E>) {
        let existing = self
            .solutions
            .iter_mut()
            .find(|s| s.rest.tokens.len() == solution.rest.tokens.len());

        if let Some(existing) = existing {
            if existing.consumed.len() < solution.consumed.len()
                || existing.value.is_none() && solution.value.is_some()
            {
                *existing = solution;
            }
        } else {
            self.solutions.push(solution);
        }
    }

    pub fn best(&self) -> Option<&Solution<'a, T, S, E>> {
        self.solutions.iter().max_by_key(|s| s.consumed.len())
    }
}

impl<'a, T, S, E> IntoIterator for Solutions<'a, T, S, E>
where
    T: Debug,
    S: Debug,
    E: Debug,
{
    type Item = Solution<'a, T, S, E>;
    type IntoIter = smallvec::IntoIter<[Solution<'a, T, S, E>; 1]>;

    fn into_iter(self) -> Self::IntoIter {
        self.solutions.into_iter()
    }
}

pub fn sequence<'a, P1, P2, T1, T2, S, E>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(Input<'a>) -> Solutions<'a, (T1, T2), S, E> + Clone
where
    T1: Clone + Debug,
    T2: Clone + Debug,
    S: Clone + Debug,
    E: FromParseError + Clone + Debug,
    P1: Fn(Input<'a>) -> Solutions<'a, T1, S, E> + Clone,
    P2: Fn(Input<'a>) -> Solutions<'a, T2, S, E> + Clone,
{
    move |input| {
        if input.fast_path {
            let solutions1 = parser1(input);
            debug_assert_eq!(solutions1.len(), 1);

            if solutions1.valid_len() == 0 {
                let mut solutions = Solutions::new();
                solutions.push(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: input,
                    value: None,
                });
                return solutions;
            }

            let solution1 = &solutions1.solutions[0];
            let solutions2 = parser2(solution1.rest);
            debug_assert_eq!(solutions2.len(), 1);
            let solution2 = &solutions2.solutions[0];

            let mut solutions = Solutions::new();

            if let (Some(value1), Some(value2)) = (&solution1.value, &solution2.value) {
                solutions.push(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: solution2.rest,
                    value: Some((value1.clone(), value2.clone())),
                });
            } else {
                solutions.push(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: input,
                    value: None,
                });
            }

            return solutions;
        }

        let mut solutions = Solutions::new();

        let solutions1 = parser1(input);
        let skip_parser1 = Solution {
            consumed: vec![],
            errors: vec![(
                (input.tokens[0].span.start..input.tokens[0].span.start).into(),
                E::missing_error("sth".to_string()),
            )],
            rest: input,
            value: None,
        };
        for solution1 in solutions1.into_iter().chain(std::iter::once(skip_parser1)) {
            let solutions2 = parser2(solution1.rest);
            for solution2 in solutions2 {
                solutions.push(Solution {
                    consumed: concat_vecs(&solution1.consumed, solution2.consumed),
                    errors: concat_vecs(&solution1.errors, solution2.errors),
                    rest: solution2.rest,
                    value: solution1.value.clone().zip(solution2.value),
                });
            }
        }

        solutions
    }
}

pub fn alt<'a, P1, P2, T, S, E>(
    parser1: P1,
    parser2: P2,
) -> impl Fn(Input<'a>) -> Solutions<'a, T, S, E> + Clone
where
    T: Clone + Debug,
    S: Clone + Debug,
    E: FromParseError + Clone + Debug,
    P1: Fn(Input<'a>) -> Solutions<'a, T, S, E> + Clone,
    P2: Fn(Input<'a>) -> Solutions<'a, T, S, E> + Clone,
{
    move |input| {
        if input.fast_path {
            let solutions1 = parser1(input);
            debug_assert_eq!(solutions1.len(), 1);

            if solutions1.valid_len() > 0 {
                return solutions1;
            }

            let solutions2 = parser2(input);
            debug_assert_eq!(solutions2.len(), 1);

            return solutions2;
        }

        let mut solutions = Solutions::new();

        let solutions1 = parser1(input);
        let solutions2 = parser2(input);

        for solution in solutions1.into_iter().chain(solutions2) {
            solutions.push(solution);
        }

        solutions
    }
}

pub fn map<'a, P, T, U, S, E, F>(
    parser: P,
    f: F,
) -> impl Fn(Input<'a>) -> Solutions<'a, U, S, E> + Clone
where
    T: Clone + Debug,
    U: Clone + Debug,
    S: Debug,
    E: Debug,
    P: Fn(Input<'a>) -> Solutions<'a, T, S, E> + Clone,
    F: Fn(T) -> U + Clone,
{
    move |input| {
        let solutions = parser(input);
        debug_assert!(!input.fast_path || solutions.len() == 1);

        let mut new_solutions = Solutions::new();

        for solution in solutions {
            let value = match solution.value {
                Some(value) => Some(f(value)),
                None => None,
            };
            new_solutions.push(Solution {
                consumed: solution.consumed,
                errors: solution.errors,
                rest: solution.rest,
                value,
            });
        }

        new_solutions
    }
}

pub fn opt<'a, P, T, S, E>(
    parser: P,
) -> impl Fn(Input<'a>) -> Solutions<'a, Option<T>, S, E> + Clone
where
    T: Clone + Debug,
    S: Clone + Debug,
    E: FromParseError + Clone + Debug,
    P: Fn(Input<'a>) -> Solutions<'a, T, S, E> + Clone,
{
    move |input| {
        if input.fast_path {
            let mut solutions = parser(input);
            debug_assert_eq!(solutions.len(), 1);
            let solution = solutions.solutions.remove(0);

            let mut new_solutions = Solutions::new();
            if let Some(value) = solution.value {
                new_solutions.push(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: solution.rest,
                    value: Some(Some(value)),
                });
            } else {
                new_solutions.push(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: input,
                    value: Some(None),
                });
            }
            return new_solutions;
        }

        let mut solutions = map(parser.clone(), Some)(input);
        solutions.push(Solution {
            consumed: vec![],
            errors: vec![],
            rest: input,
            value: Some(None),
        });
        solutions
    }
}

pub fn match_token<'a, S, E>(
    token_kind: TokenKind,
    semantic: S,
) -> impl Fn(Input<'a>) -> Solutions<'a, Token, S, E> + Clone
where
    S: Clone + Debug,
    E: FromParseError + Debug,
{
    move |input| {
        if input.fast_path {
            let mut solutions = Solutions::new();

            let token = input.tokens[0];
            if token.kind == token_kind {
                solutions.push(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: Input {
                        tokens: &input.tokens[1..],
                        fast_path: true,
                    },
                    value: Some(token.clone()),
                });
            } else {
                solutions.push(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: input,
                    value: None,
                });
            }

            return solutions;
        }

        let mut skipped_span = input.tokens[0].span;

        for (i, token) in input.tokens.iter().enumerate() {
            if token.kind == token_kind {
                if i == 0 {
                    let mut solutions = Solutions::new();
                    solutions.push(Solution {
                        consumed: vec![(token.span, semantic.clone())],
                        errors: vec![],
                        rest: Input {
                            tokens: &input.tokens[1..],
                            fast_path: false,
                        },
                        value: Some(token.clone()),
                    });
                    return solutions;
                } else {
                    let mut solutions = Solutions::new();
                    solutions.push(Solution {
                        consumed: vec![(token.span, semantic.clone())],
                        errors: vec![(skipped_span, E::match_error(token_kind))],
                        rest: Input {
                            tokens: &input.tokens[i + 1..],
                            fast_path: false,
                        },
                        value: None,
                    });
                    return solutions;
                }
            }

            skipped_span = token.span.merge(skipped_span);
        }

        let mut solutions = Solutions::new();
        solutions.push(Solution {
            consumed: vec![],
            errors: vec![(
                (input.tokens[0].span.start..input.tokens[0].span.start).into(),
                E::missing_error(format!("{:?}", token_kind)),
            )],
            rest: input,
            value: None,
        });
        solutions
    }
}

pub trait FromParseError {
    fn match_error(token_kind: TokenKind) -> Self;
    fn missing_error(label: String) -> Self;
}

fn concat_vecs<T: Clone>(a: &Vec<T>, mut b: Vec<T>) -> Vec<T> {
    b.splice(0..0, a.iter().cloned());
    b
}
