using System;
using System.Collections;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Collections
{
    public class DoubleKeyDictionary<K1, K2, V> : IEnumerable<Tuple<K1, K2, V>>
    {
        private Dictionary<K1, Dictionary<K2, V>> myDictionary = new Dictionary<K1, Dictionary<K2, V>>();

        public IEnumerable<K1> Keys1 { get { return myDictionary.Keys; } }

        public void Add(K1 key1, K2 key2, V value)
        {
            if (!myDictionary.TryGetValue(key1, out Dictionary<K2, V> dictionary2))
            {
                dictionary2 = new Dictionary<K2, V>();
                myDictionary.Add(key1, dictionary2);
            }

            dictionary2.Add(key2, value);
        }

        public bool TryGetValue(K1 key1, out Dictionary<K2, V> values)
        {
            if (myDictionary.TryGetValue(key1, out values))
            {
                return true;
            }

            return false;
        }

        public bool TryGetValue(K1 key1, K2 key2, out V value)
        {
            var result = false;
            if (TryGetValue(key1, out Dictionary<K2, V> dictionary2))
            {
                result = dictionary2.TryGetValue(key2, out value);
            }
            else
            {
                value = default(V);
            }

            return result;
        }

        public bool ContainsKey(K1 key1)
        {
            var result = myDictionary.ContainsKey(key1);
            return result;
        }

        public bool ContainsKey(K1 key1, K2 key2)
        {
            var result = false;
            if (myDictionary.TryGetValue(key1, out Dictionary<K2, V> dictionary2))
            {
                result = dictionary2.ContainsKey(key2);
            }

            return result;
        }

        public bool Remove(K1 key1)
        {
            var result = myDictionary.Remove(key1);
            return result;
        }

        public bool Remove(K1 key1, K2 key2)
        {
            var result = false;
            if (myDictionary.TryGetValue(key1, out Dictionary<K2, V> dictionary2))
            {
                result = dictionary2.Remove(key2);
            }

            return result;
        }

        public void Clear()
        {
            myDictionary.Clear();
        }

        public V this[K1 key1, K2 key2]
        {
            get
            {
                if (TryGetValue(key1, key2, out V result))
                {
                    return result;
                }

                throw new KeyNotFoundException("key1='" + key1 + "' key2 = '" + key2 + "' was not found.");
            }

            set
            {
                if (!myDictionary.TryGetValue(key1, out Dictionary<K2, V> dictionary2))
                {
                    dictionary2 = new Dictionary<K2, V>();
                    myDictionary[key1] = dictionary2;
                }

                dictionary2[key2] = value;
            }
        }

        public IEnumerator<Tuple<K1, K2, V>> GetEnumerator()
        {
            foreach (KeyValuePair<K1, Dictionary<K2, V>> item1 in myDictionary)
            {
                foreach(KeyValuePair<K2, V> aItem2 in item1.Value)
                {
                    var item = Tuple.Create(item1.Key, aItem2.Key, aItem2.Value);
                    yield return item;
                }
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
