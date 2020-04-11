using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Collections
{
    public static class EnumerableExt
    {
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
