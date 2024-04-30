use std::fmt::Debug;
use std::ops::Range;

use smallvec::SmallVec;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn merge(&self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

pub trait Token: Debug + Clone {
    type TokenKind: Debug + Copy + PartialEq;
    type TokenStream: TokenStream<Self>;

    fn span(&self) -> Span;
    fn kind(&self) -> Self::TokenKind;
}

pub trait TokenStream<T>: Debug + Clone
where T: Token
{
    fn len(&self) -> usize;
    fn take_one(&self) -> Option<(&T, Self)>;
}

impl<'a, T> TokenStream<T> for &'a [T]
where T: Token
{
    fn len(&self) -> usize {
        <[_]>::len(self)
    }

    fn take_one(&self) -> Option<(&T, &'a [T])> {
        self.split_first()
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind<TokenKind> {
    Expected(TokenKind),
    Missing(String),
}

#[derive(Debug, Clone)]
pub struct Input<T>
where T: Token
{
    pub tokens: T::TokenStream,
    pub fast_path: bool,
}

impl<T> Input<T>
where T: Token
{
    pub fn fast_path(tokens: T::TokenStream) -> Self {
        Input {
            tokens,
            fast_path: true,
        }
    }

    pub fn recovery_path(tokens: T::TokenStream) -> Self {
        Input {
            tokens,
            fast_path: false,
        }
    }
}

#[derive(Debug)]
pub struct Solution<T, O, C, E>
where T: Token
{
    pub consumed: Vec<(Span, C)>,
    pub errors: Vec<(Span, E)>,
    pub rest: Input<T>,
    pub value: Option<O>,
}

impl<T, O, C, E> Solution<T, O, C, E>
where T: Token
{
    pub fn noop(input: Input<T>) -> Self {
        Solution {
            consumed: vec![],
            errors: vec![],
            rest: input,
            value: None,
        }
    }
}

#[derive(Debug)]
pub struct IResult<T, O, C, E>
where T: Token
{
    solutions: SmallVec<[Solution<T, O, C, E>; 1]>,
}

impl<T, O, C, E> IResult<T, O, C, E>
where T: Token
{
    pub fn new() -> Self {
        IResult {
            solutions: SmallVec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.solutions.len()
    }

    pub fn push(&mut self, solution: Solution<T, O, C, E>) {
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

    pub fn best(&self) -> &Solution<T, O, C, E> {
        self.solutions
            .iter()
            .max_by_key(|s| s.consumed.len())
            .unwrap()
    }

    pub fn into_single_solution(mut self) -> Solution<T, O, C, E> {
        assert_eq!(self.len(), 1);
        self.solutions.remove(0)
    }
}

impl<T, O, C, E> IntoIterator for IResult<T, O, C, E>
where T: Token
{
    type Item = Solution<T, O, C, E>;
    type IntoIter = smallvec::IntoIter<[Solution<T, O, C, E>; 1]>;

    fn into_iter(self) -> Self::IntoIter {
        self.solutions.into_iter()
    }
}

impl<T, O, C, E> From<Solution<T, O, C, E>> for IResult<T, O, C, E>
where T: Token
{
    fn from(solution: Solution<T, O, C, E>) -> Self {
        IResult {
            solutions: smallvec::smallvec![solution],
        }
    }
}

pub trait Sequence<T, O, C, E>
where T: Token
{
    fn parse(&self, input: Input<T>) -> IResult<T, O, C, E>;
}

impl<T, O1, O2, C, E, P1, P2> Sequence<T, (O1, O2), C, E> for (P1, P2)
where
    T: Token,
    O1: Clone,
    C: Clone,
    E: From<ErrorKind<T::TokenKind>> + Clone,
    P1: Fn(Input<T>) -> IResult<T, O1, C, E>,
    P2: Fn(Input<T>) -> IResult<T, O2, C, E>,
{
    fn parse(&self, input: Input<T>) -> IResult<T, (O1, O2), C, E> {
        sequence_inner(&self.0, &self.1)(input)
    }
}

impl<T, O1, O2, O3, C, E, P1, P2, P3> Sequence<T, (O1, O2, O3), C, E> for (P1, P2, P3)
where
    T: Token,
    O1: Clone,
    O2: Clone,
    C: Clone,
    E: From<ErrorKind<T::TokenKind>> + Clone,
    P1: Fn(Input<T>) -> IResult<T, O1, C, E>,
    P2: Fn(Input<T>) -> IResult<T, O2, C, E>,
    P3: Fn(Input<T>) -> IResult<T, O3, C, E>,
{
    fn parse(&self, input: Input<T>) -> IResult<T, (O1, O2, O3), C, E> {
        map(
            sequence_inner(&self.0, &sequence_inner(&self.1, &self.2)),
            |(a, (b, c))| (a, b, c),
        )(input)
    }
}

impl<T, O1, O2, O3, O4, C, E, P1, P2, P3, P4> Sequence<T, (O1, O2, O3, O4), C, E>
    for (P1, P2, P3, P4)
where
    T: Token,
    O1: Clone,
    O2: Clone,
    O3: Clone,
    C: Clone,
    E: From<ErrorKind<T::TokenKind>> + Clone,
    P1: Fn(Input<T>) -> IResult<T, O1, C, E>,
    P2: Fn(Input<T>) -> IResult<T, O2, C, E>,
    P3: Fn(Input<T>) -> IResult<T, O3, C, E>,
    P4: Fn(Input<T>) -> IResult<T, O4, C, E>,
{
    fn parse(&self, input: Input<T>) -> IResult<T, (O1, O2, O3, O4), C, E> {
        map(
            sequence_inner(
                &self.0,
                &sequence_inner(&self.1, &sequence_inner(&self.2, &self.3)),
            ),
            |(a, (b, (c, d)))| (a, b, c, d),
        )(input)
    }
}

impl<T, O1, O2, O3, O4, O5, C, E, P1, P2, P3, P4, P5> Sequence<T, (O1, O2, O3, O4, O5), C, E>
    for (P1, P2, P3, P4, P5)
where
    T: Token,
    O1: Clone,
    O2: Clone,
    O3: Clone,
    O4: Clone,
    C: Clone,
    E: From<ErrorKind<T::TokenKind>> + Clone,
    P1: Fn(Input<T>) -> IResult<T, O1, C, E>,
    P2: Fn(Input<T>) -> IResult<T, O2, C, E>,
    P3: Fn(Input<T>) -> IResult<T, O3, C, E>,
    P4: Fn(Input<T>) -> IResult<T, O4, C, E>,
    P5: Fn(Input<T>) -> IResult<T, O5, C, E>,
{
    fn parse(&self, input: Input<T>) -> IResult<T, (O1, O2, O3, O4, O5), C, E> {
        map(
            sequence_inner(
                &self.0,
                &sequence_inner(
                    &self.1,
                    &sequence_inner(&self.2, &sequence_inner(&self.3, &self.4)),
                ),
            ),
            |(a, (b, (c, (d, e))))| (a, b, c, d, e),
        )(input)
    }
}

pub fn sequence<T, O, C, E, List>(l: List) -> impl Fn(Input<T>) -> IResult<T, O, C, E>
where
    T: Token,
    List: Sequence<T, O, C, E>,
{
    move |input| l.parse(input)
}

// pub fn sequence<T, O1, O2, C, E, P1, P2>(
//     parser1: P1,
//     parser2: P2,
// ) -> impl Fn(Input<T>) -> IResult<T, (O1, O2), C, E>
// where
//     T: Token,
//     O1: Clone,
//     C: Clone,
//     E: From<ErrorKind<T::TokenKind>> + Clone,
//     P1: Fn(Input<T>) -> IResult<T, O1, C, E>,
//     P2: Fn(Input<T>) -> IResult<T, O2, C, E>,
// {
//     move |input| {
//         if input.fast_path {
//             let solution1 = parser1(input.clone()).into_single_solution();

//             if solution1.value.is_none() {
//                 return IResult::from(Solution::noop(input.clone()));
//             }

//             let solution2 = parser2(solution1.rest).into_single_solution();

//             if let Some(value) = solution1.value.zip(solution2.value) {
//                 return IResult::from(Solution {
//                     consumed: vec![],
//                     errors: vec![],
//                     rest: solution2.rest,
//                     value: Some(value),
//                 });
//             } else {
//                 return IResult::from(Solution::noop(input));
//             }
//         }

//         let mut solutions = IResult::new();

//         let solutions1 = parser1(input.clone());
//         let recovery_skip_parser1 = input.tokens.take_one().map(|(token, _)| {
//             let span = token.span();
//             Solution {
//                 consumed: vec![],
//                 errors: vec![(
//                     (span.start..span.start).into(),
//                     ErrorKind::Missing("sth".to_string()).into(),
//                 )],
//                 rest: input.clone(),
//                 value: None,
//             }
//         });
//         for solution1 in solutions1.into_iter().chain(recovery_skip_parser1) {
//             let solutions2 = parser2(solution1.rest);
//             for solution2 in solutions2 {
//                 solutions.push(Solution {
//                     consumed: concat_vecs(&solution1.consumed, solution2.consumed),
//                     errors: concat_vecs(&solution1.errors, solution2.errors),
//                     rest: solution2.rest,
//                     value: solution1.value.clone().zip(solution2.value),
//                 });
//             }
//         }

//         solutions
//     }
// }

fn sequence_inner<'a, T, O1, O2, C, E, P1, P2>(
    parser1: &'a P1,
    parser2: &'a P2,
) -> impl Fn(Input<T>) -> IResult<T, (O1, O2), C, E> + 'a
where
    T: Token,
    O1: Clone,
    C: Clone,
    E: From<ErrorKind<T::TokenKind>> + Clone,
    P1: Fn(Input<T>) -> IResult<T, O1, C, E>,
    P2: Fn(Input<T>) -> IResult<T, O2, C, E>,
{
    move |input| {
        if input.fast_path {
            let solution1 = parser1(input.clone()).into_single_solution();

            if solution1.value.is_none() {
                return IResult::from(Solution::noop(input.clone()));
            }

            let solution2 = parser2(solution1.rest).into_single_solution();

            if let Some(value) = solution1.value.zip(solution2.value) {
                return IResult::from(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: solution2.rest,
                    value: Some(value),
                });
            } else {
                return IResult::from(Solution::noop(input));
            }
        }

        let mut solutions = IResult::new();

        let solutions1 = parser1(input.clone());
        let recovery_skip_parser1 = input.tokens.take_one().map(|(token, _)| {
            let span = token.span();
            Solution {
                consumed: vec![],
                errors: vec![(
                    (span.start..span.start).into(),
                    ErrorKind::Missing("sth".to_string()).into(),
                )],
                rest: input.clone(),
                value: None,
            }
        });
        for solution1 in solutions1.into_iter().chain(recovery_skip_parser1) {
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

pub fn alt<T, O, C, E, P1, P2>(parser1: P1, parser2: P2) -> impl Fn(Input<T>) -> IResult<T, O, C, E>
where
    T: Token,
    P1: Fn(Input<T>) -> IResult<T, O, C, E>,
    P2: Fn(Input<T>) -> IResult<T, O, C, E>,
{
    move |input| {
        if input.fast_path {
            let solution1 = parser1(input.clone()).into_single_solution();
            if solution1.value.is_some() {
                return IResult::from(solution1);
            }

            let solution2 = parser2(input.clone()).into_single_solution();
            return IResult::from(solution2);
        }

        let solutions1 = parser1(input.clone());
        let solutions2 = parser2(input);

        let mut solutions = IResult::new();
        for solution in solutions1.into_iter().chain(solutions2) {
            solutions.push(solution);
        }

        solutions
    }
}

pub fn map<T, O1, O2, C, E, P, F>(parser: P, f: F) -> impl Fn(Input<T>) -> IResult<T, O2, C, E>
where
    T: Token,
    P: Fn(Input<T>) -> IResult<T, O1, C, E>,
    F: Fn(O1) -> O2,
{
    move |input| {
        let solutions = parser(input.clone());
        debug_assert!(!input.fast_path || solutions.len() == 1);

        let mut new_solutions = IResult::new();

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

pub fn opt<T, O, C, E, P>(parser: P) -> impl Fn(Input<T>) -> IResult<T, Option<O>, C, E>
where
    T: Token,
    P: Fn(Input<T>) -> IResult<T, O, C, E>,
{
    move |input| {
        if input.fast_path {
            let solution = parser(input.clone()).into_single_solution();
            if let Some(value) = solution.value {
                return IResult::from(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: solution.rest,
                    value: Some(Some(value)),
                });
            } else {
                return IResult::from(Solution {
                    consumed: vec![],
                    errors: vec![],
                    rest: input,
                    value: Some(None),
                });
            }
        }

        let solutions = parser(input.clone());

        let mut new_solutions = IResult::new();
        for solution in solutions {
            let value = match solution.value {
                Some(value) => Some(Some(value)),
                None => None,
            };
            new_solutions.push(Solution {
                consumed: solution.consumed,
                errors: solution.errors,
                rest: solution.rest,
                value,
            });
        }
        new_solutions.push(Solution {
            consumed: vec![],
            errors: vec![],
            rest: input,
            value: Some(None),
        });

        new_solutions
    }
}

pub fn match_token<T, C, E>(
    token_kind: T::TokenKind,
    color: C,
) -> impl Fn(Input<T>) -> IResult<T, T, C, E>
where
    T: Token,
    C: Clone,
    E: From<ErrorKind<T::TokenKind>>,
{
    move |input| {
        if input.fast_path {
            if let Some((token, rest)) = input.tokens.take_one() {
                if token.kind() == token_kind {
                    return IResult::from(Solution {
                        consumed: vec![],
                        errors: vec![],
                        rest: Input {
                            tokens: rest,
                            ..input
                        },
                        value: Some(token.clone()),
                    });
                }
            }

            return IResult::from(Solution::noop(input));
        }

        let mut skipped_span = None;
        let mut tokens = input.tokens.clone();
        while let Some((token, rest)) = tokens.take_one() {
            let span = token.span();

            if token.kind() == token_kind {
                match skipped_span {
                    Some(skipped_span) => {
                        return IResult::from(Solution {
                            consumed: vec![(span, color.clone())],
                            errors: vec![(skipped_span, ErrorKind::Expected(token_kind).into())],
                            rest: Input {
                                tokens: rest,
                                ..input
                            },
                            value: None,
                        });
                    }
                    None => {
                        return IResult::from(Solution {
                            consumed: vec![(span, color.clone())],
                            errors: vec![],
                            rest: Input {
                                tokens: rest,
                                ..input
                            },
                            value: Some(token.clone()),
                        });
                    }
                }
            }

            skipped_span = Some(skipped_span.unwrap_or(span).merge(span));
            tokens = rest;
        }

        if let Some((token, _)) = input.tokens.take_one() {
            let span = token.span();
            IResult::from(Solution {
                consumed: vec![],
                errors: vec![(
                    (span.start..span.start).into(),
                    ErrorKind::Expected(token_kind).into(),
                )],
                rest: input,
                value: None,
            })
        } else {
            IResult::from(Solution {
                consumed: vec![],
                errors: vec![],
                rest: input,
                value: None,
            })
        }
    }
}

fn concat_vecs<O: Clone>(a: &Vec<O>, mut b: Vec<O>) -> Vec<O> {
    b.splice(0..0, a.iter().cloned());
    b
}
