#![recursion_limit = "512"]
use std::marker::*;

struct Zero;

struct Succ<N>(PhantomData<N>);

trait Eq<N, T, F> {
    type R;
}

impl<T, F> Eq<Zero, T, F> for Zero {
    type R = T;
}

impl<N, T, F> Eq<Succ<N>, T, F> for Zero {
    type R = F;
}

impl<N, T, F> Eq<Zero, T, F> for Succ<N> {
    type R = F;
}

impl<A: Eq<B, T, F>, B, T, F> Eq<Succ<A>, T, F> for Succ<B> {
    type R = <A as Eq<B, T, F>>::R;
}

struct Var<N>(PhantomData<N>);

struct Abs<N, Body>(PhantomData<N>, PhantomData<Body>);

struct App<E1, E2>(PhantomData<E1>, PhantomData<E2>);

trait Subst<X, T> {
    type R;
}

impl<X, Y, T> Subst<X, T> for Var<Y>
where
    X: Eq<Y, T, Var<Y>>,
{
    type R = <X as Eq<Y, T, Var<Y>>>::R;
}

impl<X, T, N, Body> Subst<X, T> for Abs<N, Body>
where
    X: Eq<N, Body, Abs<N, <Body as Subst<X, T>>::R>>,
    Body: Subst<X, T>,
{
    type R = <X as Eq<N, Body, Abs<N, <Body as Subst<X, T>>::R>>>::R;
}

impl<X, T, E1, E2> Subst<X, T> for App<E1, E2>
where
    E1: Subst<X, T>,
    E2: Subst<X, T>,
{
    type R = App<<E1 as Subst<X, T>>::R, <E2 as Subst<X, T>>::R>;
}

type One = Succ<Zero>;

struct True;
struct False;

trait Value<T, F> {
    type R;
}

impl<X, T, F> Value<T, F> for Var<X> {
    type R = T;
}

impl<X, Body, T, F> Value<T, F> for Abs<X, Body> {
    type R = T;
}

impl<T, F, E1, E2> Value<T, F> for App<E1, E2> {
    type R = F;
}

trait Apply<Argument> {
    type R;
}

impl<Arg, X> Apply<Arg> for Var<X> {
    type R = Var<X>;
}

impl<Arg, X, Body> Apply<Arg> for Abs<X, Body>
where
    Body: Subst<X, Arg>,
{
    type R = <Body as Subst<X, Arg>>::R;
}

impl<Arg, E1, E2> Apply<Arg> for App<E1, E2> {
    type R = App<E1, E2>;
}

trait Step {
    type R;
}

impl<X> Step for Var<X> {
    type R = Var<X>;
}

impl<X, Body> Step for Abs<X, Body> {
    type R = Abs<X, Body>;
}

impl<E1, E2> Step for App<E1, E2>
where
    E1: Step
        + Apply<E2>
        + Value<<E2 as Value<<E1 as Apply<E2>>::R, <E2 as Step>::R>>::R, <E1 as Step>::R>,
    E2: Step + Value<<E1 as Apply<E2>>::R, <E2 as Step>::R>,
{
    type R =
        <E1 as Value<<E2 as Value<<E1 as Apply<E2>>::R, <E2 as Step>::R>>::R, <E1 as Step>::R>>::R;
}

type ExFn = Abs<Zero, Var<Zero>>;
type ExApp = App<ExFn, Var<One>>;
type Example = <ExApp as Step>::R;

fn main() {
    let mut x: Example = Var(PhantomData);
    let y: Var<One> = Var(PhantomData);
    x = y;
}
