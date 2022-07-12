using Krino.Vertical.Utils.Rules;
using System;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Helper functionality to build a composition of transformations.
    /// </summary>
    public static class Trans
    {
        public static AggregateTransformation<T> Aggregate<T>(params ITransformation<T>[] transformations) => new AggregateTransformation<T>(transformations);

        /// <summary>
        /// Returns the same not changed value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static ITransformation<T> NothingToDo<T>() => new NothingToDoTransformation<T>();

        /// <summary>
        /// Replaces the value with the new value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="newValue"></param>
        /// <returns></returns>
        public static ITransformation<T> ReplaceWith<T>(T newValue) => new ReplaceWithTransformation<T>(newValue);

        /// <summary>
        /// Concatenates the next transformation.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="t1"></param>
        /// <param name="t2"></param>
        /// <returns></returns>
        public static ITransformation<T> Aggregate<T>(this ITransformation<T> t1, ITransformation<T> t2) => new AggregateTransformation<T>(t1, t2);

        /// <summary>
        /// Performs the transformation if the condition is met.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="condition"></param>
        /// <param name="then"></param>
        /// <returns></returns>
        public static IfElseTransformation<T> If<T>(IRule<T> condition, ITransformation<T> then) => new IfElseTransformation<T>(condition, then, null);

        /// <summary>
        /// The 'else' part of the conditional transformation.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="t"></param>
        /// <param name="otherwise"></param>
        /// <returns></returns>
        public static IfElseTransformation<T> Else<T>(this IfElseTransformation<T> t, ITransformation<T> otherwise) => IfElseTransformation<T>.Else(t, otherwise);

        public static IfElseTransformation<T> ElseIf<T>(this IfElseTransformation<T> t, IRule<T> condition, ITransformation<T> then) => IfElseTransformation<T>.Else(t, If(condition, then));

        public static AppendTransformation<T> Append<T>(T toAppend, Func<T, T, T> onAppend) => new AppendTransformation<T>(toAppend, onAppend);

        public static AppendTransformation<string> Append(string toAppend) => new AppendTransformation<string>(toAppend, (value, append) => string.Concat(value, append));

        public static PrependTransformation<T> Prepend<T>(T toPrepend, Func<T, T, T> onPrepend) => new PrependTransformation<T>(toPrepend, onPrepend);

        public static PrependTransformation<string> Prepend(string toPrepend) => new PrependTransformation<string>(toPrepend, (prepend, value) => string.Concat(prepend, value));
    }
}
