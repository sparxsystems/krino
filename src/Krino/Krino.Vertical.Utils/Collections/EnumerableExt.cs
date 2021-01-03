using Krino.Vertical.Utils.Strings;
using System;
using System.Collections.Generic;
using System.ComponentModel;
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

        /// <summary>
        /// Returns true if the sequence has exactly one item.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="source"></param>
        /// <returns></returns>
        public static bool IsSingle<T>(this IEnumerable<T> source)
        {
            bool aResult = source.Take(2).Count() == 1;
            return aResult;
        }

        /// <summary>
        /// Returns variation of possible sequences.
        /// </summary>
        /// <remarks>
        /// E.g. if source = ( (1, 2), (10, 20) ) then result = ( (1, 2), (10, 2), (1, 20), (10, 20) ).
        /// </remarks>
        /// <typeparam name="T"></typeparam>
        /// <param name="source"></param>
        /// <returns></returns>
        public static IEnumerable<IEnumerable<T>> GetVariations<T>(this IEnumerable<IEnumerable<T>> source)
        {
            List<IEnumerator<T>> enumerators = source.Select(x =>
                {
                    IEnumerator<T> enumerator = x.GetEnumerator();

                    // Set the enumerator to the first item.
                    if (!enumerator.MoveNext())
                    {
                        throw new InvalidEnumArgumentException("The sequences inside the source cannot be empty.");
                    }
                    return enumerator;
                }).ToList();

            bool isCompleted = false;
            while (!isCompleted)
            {
                IEnumerable<T> variation = enumerators.Select(x => x.Current);
                yield return variation;

                // Try to set enumerators to represent the next variation.
                isCompleted = true;
                foreach (IEnumerator<T> enumerator in enumerators)
                {
                    if (enumerator.MoveNext())
                    {
                        isCompleted = false;
                        break;
                    }

                    // Reset to the first item.
                    enumerator.Reset();
                    enumerator.MoveNext();
                }
            }
        }
    }
}
