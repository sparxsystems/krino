using Krino.Vertical.Utils.Rules;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Helper functionality to build a composition of transformations.
    /// </summary>
    public static class Trans
    {
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
        public static ITransformation<T> ContinueWith<T>(this ITransformation<T> t1, ITransformation<T> t2) => new ContinueWithTransformation<T>(t1, t2);

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
        public static ITransformation<T> Else<T>(this IfElseTransformation<T> t, ITransformation<T> otherwise) => IfElseTransformation<T>.Else(t, otherwise);
    }
}
