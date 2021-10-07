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

trait Inc {
    type R;
}

impl Inc for Zero {
    type R = Succ<Zero>;
}

impl<N> Inc for Succ<N> {
    type R = Succ<Succ<N>>;
}

trait Plus<N> {
    type R;
}

impl<A> Plus<A> for Zero {
    type R = A;
}

/*
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ a) b =
  if iszero b then
    Succ a
  else Succ (a `add` b)
*/
impl<A, B> Plus<B> for Succ<A>
where
    A: Plus<B>,
    B: IsZero<Succ<A>, Succ<<A as Plus<B>>::R>>,
{
    type R = <B as IsZero<Succ<A>, Succ<<A as Plus<B>>::R>>>::R;
}

trait Decr {
    type R;
}

impl Decr for Zero {
    type R = Zero;
}

impl<N> Decr for Succ<N> {
    type R = N;
}

trait IsZero<T, F> {
    type R;
}

impl<T, F> IsZero<T, F> for Zero {
    type R = T;
}

impl<T, F, N> IsZero<T, F> for Succ<N> {
    type R = F;
}

trait IsOne<T, F> {
    type R;
}

impl<T, F> IsOne<T, F> for Zero {
    type R = F;
}

impl<T, F, N> IsOne<T, F> for Succ<N>
where
    N: IsZero<T, F>,
{
    type R = <N as IsZero<T, F>>::R;
}

trait Multiply<N> {
    type R;
}

impl<N> Multiply<N> for Zero {
    type R = Zero;
}

/*
multiply :: Nat -> Nat -> Nat
multiply Zero a = Zero
multiply (Succ a) b =
  if iszero a then
    b
  else if iszero b then
    Zero
  else
    b `add` (a `multiply` b)
*/

impl<A, B> Multiply<B> for Succ<A>
where
    A: Plus<B> + Multiply<B>,
    B: Plus<<A as Multiply<B>>::R>,
    B: IsZero<Zero, <B as Plus<<A as Multiply<B>>::R>>::R>,
    A: IsZero<B, <B as IsZero<Zero, <B as Plus<<A as Multiply<B>>::R>>::R>>::R>,
{
    type R = <A as IsZero<B, <B as IsZero<Zero, <B as Plus<<A as Multiply<B>>::R>>::R>>::R>>::R;
}

struct Var<N>(PhantomData<N>);

struct Abs<N, Body>(PhantomData<N>, PhantomData<Body>);

struct App<E1, E2>(PhantomData<E1>, PhantomData<E2>);

struct If0<G, E1, E2>(PhantomData<G>, PhantomData<E1>, PhantomData<E2>);

struct Sub1<E>(PhantomData<E>);

struct Mult<E1, E2>(PhantomData<E1>, PhantomData<E2>);

trait Subst<X, T> {
    type R;
}

impl<X, Y, T> Subst<X, T> for Var<Y>
where
    X: Eq<Y, T, Var<Y>>,
{
    type R = <X as Eq<Y, T, Var<Y>>>::R;
}

impl<X, T> Subst<X, T> for Zero {
    type R = Zero;
}

impl<X, T, N> Subst<X, T> for Succ<N>
where
    N: Subst<X, T>,
{
    type R = Succ<<N as Subst<X, T>>::R>;
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

impl<X, T, E> Subst<X, T> for Sub1<E>
where
    E: Subst<X, T>,
{
    type R = Sub1<<E as Subst<X, T>>::R>;
}

impl<X, T, G, E1, E2> Subst<X, T> for If0<G, E1, E2>
where
    G: Subst<X, T>,
    E1: Subst<X, T>,
    E2: Subst<X, T>,
{
    type R = If0<<G as Subst<X, T>>::R, <E1 as Subst<X, T>>::R, <E2 as Subst<X, T>>::R>;
}

impl<X, T, E1, E2> Subst<X, T> for Mult<E1, E2>
where
    E1: Subst<X, T>,
    E2: Subst<X, T>,
{
    type R = Mult<<E1 as Subst<X, T>>::R, <E2 as Subst<X, T>>::R>;
}

struct True;
struct False;

trait Value<T, F> {
    type R;
}

impl<X, T, F> Value<T, F> for Var<X> {
    type R = T;
}

impl<T, F> Value<T, F> for Zero {
    type R = T;
}

impl<T, F, N> Value<T, F> for Succ<N>
where
    N: Value<T, F>,
{
    type R = <N as Value<T, F>>::R;
}

impl<T, F, E> Value<T, F> for Sub1<E> {
    type R = F;
}

impl<T, F, G, E1, E2> Value<T, F> for If0<G, E1, E2> {
    type R = F;
}

impl<T, F, E1, E2> Value<T, F> for Mult<E1, E2> {
    type R = F;
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

impl Step for Zero {
    type R = Zero;
}

impl<N> Step for Succ<N>
where
    N: Step,
{
    type R = Succ<<N as Step>::R>;
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

impl<E> Step for Sub1<E>
where
    E: Decr + Step + Value<<E as Decr>::R, <E as Step>::R>,
{
    type R = <E as Value<
        <E as Decr>::R,
        // Not a value
        <E as Step>::R,
    >>::R;
}

impl<E1, E2> Step for Mult<E1, E2>
where
    E1: Multiply<E2>
        + Step
        + Value<<E2 as Value<<E1 as Multiply<E2>>::R, <E2 as Step>::R>>::R, <E1 as Step>::R>,
    E2: Step + Value<<E1 as Multiply<E2>>::R, <E2 as Step>::R>,
{
    type R = <E1 as Value<
        <E2 as Value<
            <E1 as Multiply<E2>>::R,
            // E2 is not a value
            <E2 as Step>::R,
        >>::R,
        //E1 is not a value
        <E1 as Step>::R,
    >>::R;
}

trait IsNumber<T, F> {
    type R;
}

impl<T, F> IsNumber<T, F> for Zero {
    type R = T;
}

impl<T, F, N> IsNumber<T, F> for Succ<N> {
    type R = T;
}

impl<T, F, N, B> IsNumber<T, F> for Abs<N, B> {
    type R = F;
}

type A = Zero;
type Two = Succ<Succ<Zero>>;
type One = Succ<Zero>;
type Three = Succ<Succ<Succ<Zero>>>;
type Six = Succ<Succ<Succ<Succ<Succ<Succ<Zero>>>>>>;
type Five = Succ<Succ<Succ<Succ<Succ<Zero>>>>>;

type TimesTwo = Abs<A, Mult<Var<A>, Two>>;
type Example = App<TimesTwo, Three>;

#[allow(unused_assignments)]
#[allow(unused_variables)]
fn main() {
    println!("Hello world");
    let ex: <Example as Step>::R = Mult(PhantomData, PhantomData);
    let ex_: Mult<Three, Two> = ex;
    let m: <Three as Multiply<Two>>::R = Succ(PhantomData);
    let m_: Six = m;
    let ex__: <<Example as Step>::R as Step>::R = Succ(PhantomData);
    let result: Six = ex__;
}
