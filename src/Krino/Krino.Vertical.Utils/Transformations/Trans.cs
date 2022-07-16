using Krino.Vertical.Utils.Rules;
using System;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Helper functionality to build a composition of transformations.
    /// </summary>
    public static class Trans
    {
        public static BlockTransformation<T> Block<T>(params ITransformation<T>[] transformations) => new BlockTransformation<T>(transformations);

        /// <summary>
        /// Returns the same not changed value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static NothingToDoTransformation<T> NothingToDo<T>() => new NothingToDoTransformation<T>();

        /// <summary>
        /// Replaces the value with the new value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="newValue"></param>
        /// <returns></returns>
        public static ReplaceWithTransformation<T> ReplaceWith<T>(T newValue) => new ReplaceWithTransformation<T>(newValue);

        /// <summary>
        /// Concatenates the next transformation.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="t1"></param>
        /// <param name="t2"></param>
        /// <returns></returns>
        public static BlockTransformation<T> ContinueWith<T>(this ITransformation<T> t1, ITransformation<T> t2) => new BlockTransformation<T>(t1, t2);

        /// <summary>
        /// Performs the transformation if the condition is met.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="condition"></param>
        /// <param name="then"></param>
        /// <returns></returns>
        public static IfElseTransformation<T> If<T>(IRule<T> condition, params ITransformation<T>[] then)
        {
            IfElseTransformation<T> result;

            if (then.Length == 1)
            {
                result = new IfElseTransformation<T>(condition, then[0], null);
            }
            else
            {
                var block = Block(then);
                result = new IfElseTransformation<T>(condition, block, null);
            }

            return result;
        }

        /// <summary>
        /// The 'else' part of the conditional transformation.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="t"></param>
        /// <param name="otherwise"></param>
        /// <returns></returns>
        public static IfElseTransformation<T> Else<T>(this IfElseTransformation<T> t, ITransformation<T> otherwise) => IfElseTransformation<T>.Else(t, otherwise);


        public static AppendTransformation<T> Append<T>(T toAppend, Func<T, T, T> onAppend) => new AppendTransformation<T>(toAppend, onAppend);

        public static AppendTransformation<string> Append(string toAppend) => Append(toAppend, (value, append) => string.Concat(value, append));

        public static PrependTransformation<T> Prepend<T>(T toPrepend, Func<T, T, T> onPrepend) => new PrependTransformation<T>(toPrepend, onPrepend);

        public static PrependTransformation<string> Prepend(string toPrepend) => Prepend(toPrepend, (prepend, value) => string.Concat(prepend, value));

        public static DropTransformation<T> Drop<T>(int startIdx, int count, Func<T, int, int, T> onDrop) => new DropTransformation<T>(startIdx, count, onDrop);

        public static DropTransformation<string> DropFirst(int startIdx, int count) => Drop<string>(startIdx, count, TryDropFromStart);
        public static DropTransformation<string> DropFirst(int startIdx) => DropFirst(startIdx, int.MaxValue);
        public static DropTransformation<string> DropFromEnd(int endIdx, int count) => Drop<string>(endIdx, count, TryDropFromEnd);
        public static DropTransformation<string> DropFromEnd(int endIdx) => DropFromEnd(endIdx, int.MaxValue);

        public static ReplaceTransformation<T> Replace<T>(T oldValue, T newValue, Func<T, T, T, T> onReplace) => new ReplaceTransformation<T>(oldValue, newValue, onReplace);
        public static ReplaceTransformation<string> Replace(string oldValue, string newValue) => Replace(oldValue, newValue, (src, oldValue, newValue) => src?.Replace(oldValue, newValue));


        private static string TryDropFromStart(string str, int startIdx, int count)
        {
            string result = str;

            if (str != null && startIdx < str.Length)
            {
                int countToUse = startIdx + count <= str.Length ? count : str.Length - startIdx;
                result = str.Remove(startIdx, countToUse);
            }

            return result;
        }

        private static string TryDropFromEnd(string str, int endIdx, int count)
        {
            string result = str;

            if (str != null && str.Length - endIdx >= 0)
            {
                if (str.Length - endIdx - count >= 0)
                {
                    int startIdx = str.Length - endIdx - count;
                    int countToUse = count;
                    result = str.Remove(startIdx, countToUse);
                }
                else
                {
                    result = str.Substring(str.Length - endIdx);
                }
            }

            return result;
        }
    }
}
