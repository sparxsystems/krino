using System.Collections.Generic;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Extension methods for ICollection
    /// </summary>
    public static class CollectionExt
    {
        /// <summary>
        /// Adds the range into the collection.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="o"></param>
        /// <param name="items"></param>
        public static void AddRange<T>(this ICollection<T> o, IEnumerable<T> items)
        {
            foreach (T item in items)
            {
                o.Add(item);
            }
        }
    }
}
