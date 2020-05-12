using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Dictionary where each key can have multiple distinct values.
    /// </summary>
    /// <typeparam name="K"></typeparam>
    /// <typeparam name="V"></typeparam>
    public class MultiKeyDistinctValueDictionary<K, V> : IEnumerable<KeyValuePair<K, V>>
    {
        private Dictionary<K, HashSet<V>> myDictionary;
        private IEqualityComparer<V> myValueComparer;

        public MultiKeyDistinctValueDictionary(IEqualityComparer<K> keyComparer = null, IEqualityComparer<V> valueComparer = null)
        {
            myDictionary = new Dictionary<K, HashSet<V>>(keyComparer ?? EqualityComparer<K>.Default);
            myValueComparer = valueComparer ?? EqualityComparer<V>.Default;
        }

        public int Count { get; private set; }

        public ReadOnlySet<V> this[K key] => new ReadOnlySet<V>(myDictionary[key]);

        public IEnumerable<K> Keys => myDictionary.Keys;

        public bool Add(K key, V value)
        {
            myDictionary.TryGetValue(key, out HashSet<V> values);
            if (values == null)
            {
                values = new HashSet<V>(myValueComparer);
                myDictionary[key] = values;
            }

            if (values.Add(value))
            {
                ++Count;
                return true;
            }

            return false;
        }

        public void Add(K key, IEnumerable<V> values)
        {
            HashSet<V> setToAdd = new HashSet<V>(values, myValueComparer);
            myDictionary.Add(key, setToAdd);
            Count += setToAdd.Count;
        }


        /// <summary>
        /// Removes the key and all its values.
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool Remove(K key)
        {
            myDictionary.TryGetValue(key, out HashSet<V> values);
            if (values != null)
            {
                myDictionary.Remove(key);

                Count -= values.Count;
                return true;
            }

            return false;
        }

        /// <summary>
        /// Removes the value from the specified key.
        /// </summary>
        /// <param name="key"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool Remove(K key, V value)
        {
            myDictionary.TryGetValue(key, out HashSet<V> values);
            if (values != null)
            {
                if (values.Remove(value))
                {
                    if (values.Count == 0)
                    {
                        myDictionary.Remove(key);
                    }

                    --Count;
                    return true;
                }
            }

            return false;
        }

        public bool TryGetValues(K key, out ReadOnlySet<V> values)
        {
            values = null;

            myDictionary.TryGetValue(key, out HashSet<V> foundValues);
            if (foundValues != null)
            {
                values = new ReadOnlySet<V>(foundValues);
            }

            return values != null;
        }

        public bool ContainsKey(K key) => myDictionary.ContainsKey(key);

        public bool Contains(K key, V value)
        {
            bool result = false;

            myDictionary.TryGetValue(key, out HashSet<V> values);
            if (values != null)
            {
                result = values.Contains(value);
            }

            return result;
        }

        public IEnumerator<KeyValuePair<K, V>> GetEnumerator()
        {
            IEnumerator<KeyValuePair<K, V>> result = myDictionary.SelectMany(x => x.Value.Select(y => new KeyValuePair<K, V>(x.Key, y)))
                .GetEnumerator();

            return result;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
