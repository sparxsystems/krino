using Krino.Vertical.Utils.Strings;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Utility and extending functionality for IEnumerable.
    /// </summary>
    public static class EnumerableExt
    {
        /// <summary>
        /// Finds all similar strings using Levenshtein algorithm.
        /// </summary>
        /// <param name="source"></param>
        /// <param name="value"></param>
        /// <param name="maxDistance"></param>
        /// <returns></returns>
        public static IEnumerable<string> FindSimilar(this IEnumerable<string> source, string value, int maxDistance)
        {
            IEnumerable<string> result = source.Where(x => value.Distance(x) <= maxDistance);
            return result;
        }

        /// <summary>
        /// Same as TakeWhile() but also returns the first element which broke the condition.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="source"></param>
        /// <param name="predicate"></param>
        /// <returns></returns>
        public static IEnumerable<T> TakeUntil<T>(this IEnumerable<T> source, Predicate<T> predicate)
        {
            foreach (T item in source)
            {
                bool shallContinue = predicate(item);

                yield return item;

                if (!shallContinue)
                {
                    break;
                }
            }
        }
    }
}
