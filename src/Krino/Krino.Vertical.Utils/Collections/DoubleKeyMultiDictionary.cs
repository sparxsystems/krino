using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Collections
{
    public class DoubleKeyMultiDictionary<K1, K2, V> : IEnumerable<Tuple<K1, K2, V>>
    {
        private IEqualityComparer<K1> myK1Comparer;
        private IEqualityComparer<K2> myK2Comparer;
        private IEqualityComparer<V> myValueComparer;

        private Dictionary<K1, List<KeyValuePair<K2, V>>> myK1Values = new Dictionary<K1, List<KeyValuePair<K2, V>>>();
        private Dictionary<K2, List<KeyValuePair<K1, V>>> myK2Values = new Dictionary<K2, List<KeyValuePair<K1, V>>>();

        public IEnumerable<K1> Keys1 { get { return myK1Values.Keys; } }
        public IEnumerable<K2> Keys2 { get { return myK2Values.Keys; } }

        public int Count => myK1Values.Sum(x => x.Value.Count);

        public DoubleKeyMultiDictionary(IEqualityComparer<K1> k1Comparer = null, IEqualityComparer<K2> k2Comparer = null, IEqualityComparer<V> valueComparer = null)
        {
            myK1Comparer = k1Comparer ?? EqualityComparer<K1>.Default;
            myK2Comparer = k2Comparer ?? EqualityComparer<K2>.Default;
            myValueComparer = valueComparer ?? EqualityComparer<V>.Default;

            myK1Values = new Dictionary<K1, List<KeyValuePair<K2, V>>>(myK1Comparer);
            myK2Values = new Dictionary<K2, List<KeyValuePair<K1, V>>>(myK2Comparer);
        }

        public void Add(K1 key1, K2 key2, V value)
        {
            if (!myK1Values.TryGetValue(key1, out List<KeyValuePair<K2, V>> k1Values))
            {
                k1Values = new List<KeyValuePair<K2, V>>();
                myK1Values.Add(key1, k1Values);
            }
            if (!myK2Values.TryGetValue(key2, out List<KeyValuePair<K1, V>> k2Values))
            {
                k2Values = new List<KeyValuePair<K1, V>>();
                myK2Values.Add(key2, k2Values);
            }

            k1Values.Add(new KeyValuePair<K2, V>(key2, value));
            k2Values.Add(new KeyValuePair<K1, V>(key1, value));
        }

        public IReadOnlyList<KeyValuePair<K2, V>> GetValuesForKey1(K1 key1)
        {
            if (!myK1Values.TryGetValue(key1, out List<KeyValuePair<K2, V>> result))
            {
                result = new List<KeyValuePair<K2, V>>(0);
            }

            return result;
        }

        public IReadOnlyList<KeyValuePair<K1, V>> GetValuesForKey2(K2 key2)
        {
            if (!myK2Values.TryGetValue(key2, out List<KeyValuePair<K1, V>> result))
            {
                result = new List<KeyValuePair<K1, V>>(0);
            }

            return result;
        }

        public IEnumerable<V> GetValues(K1 key1, K2 key2)
        {
            IEnumerable<KeyValuePair<K2, V>> k2Values = GetValuesForKey1(key1);
            IEnumerable<V> result = k2Values.Where(x => myK2Comparer.Equals(key2, x.Key)).Select(x => x.Value);
            return result;
        }

        public bool ContainsKey1(K1 key1)
        {
            bool aResult = myK1Values.ContainsKey(key1);
            return aResult;
        }

        public bool ContainsKey2(K2 key2)
        {
            bool aResult = myK2Values.ContainsKey(key2);
            return aResult;
        }

        public bool RemoveForKey1(K1 key1, Func<K1, K2, V, bool> predicate = null)
        {
            bool result = false;

            if (myK1Values.ContainsKey(key1))
            {
                HashSet<K2> k2Values = GetValuesForKey1(key1).Select(x => x.Key).ToHashSet(myK2Comparer);
                Dictionary<K1, HashSet<K2>> toRemove = new Dictionary<K1, HashSet<K2>>(1, myK1Comparer) { { key1, k2Values } };
                result = Remove(toRemove, myK1Values, myK2Values, myK1Comparer, predicate);
            }

            return result;
        }

        public bool RemoveForKey2(K2 key2, Func<K1, K2, V, bool> predicate = null)
        {
            bool result = false;

            if (myK2Values.ContainsKey(key2))
            {
                HashSet<K1> k1Values = GetValuesForKey2(key2).Select(x => x.Key).ToHashSet(myK1Comparer);
                Dictionary<K2, HashSet<K1>> toRemove = new Dictionary<K2, HashSet<K1>>(1, myK2Comparer) { { key2, k1Values } };

                // Note: it is other way around in this case.
                if (predicate != null)
                {
                    result = Remove(toRemove, myK2Values, myK1Values, myK2Comparer, (k2, k1, v) => predicate(k1, k2, v));

                }
                else
                {
                    result = Remove(toRemove, myK2Values, myK1Values, myK2Comparer);
                }
            }

            return result;
        }

        public bool Remove(K1 key1, K2 key2, Func<K1, K2, V, bool> predicate = null)
        {
            Dictionary<K1, HashSet<K2>> toRemove = new Dictionary<K1, HashSet<K2>>(myK1Comparer) { { key1, new HashSet<K2>(1, myK2Comparer) { key2 } } };
            bool result = Remove(toRemove, myK1Values, myK2Values, myK1Comparer, predicate);
            return result;
        }

        public bool Remove(IEnumerable<Tuple<K1, K2>> toRemove, Func<K1, K2, V, bool> predicate = null)
        {
            IEnumerable<IGrouping<K1, K2>> toRemoveGrouped = toRemove.GroupBy(x => x.Item1, x => x.Item2, myK1Comparer);
            Dictionary<K1, HashSet<K2>> toRemoveHashedGroups = toRemoveGrouped.ToDictionary(x => x.Key, x => x.ToHashSet(myK2Comparer), myK1Comparer);
            bool result = Remove(toRemoveHashedGroups, myK1Values, myK2Values, myK1Comparer, predicate);
            return result;
        }

        public void Clear()
        {
            myK1Values.Clear();
            myK2Values.Clear();
        }

        public IEnumerator<Tuple<K1, K2, V>> GetEnumerator()
        {
            foreach (KeyValuePair<K1, List<KeyValuePair<K2, V>>> k1Item in myK1Values)
            {
                if (k1Item.Value != null)
                {
                    foreach (KeyValuePair<K2, V> k2Item in k1Item.Value)
                    {
                        Tuple<K1, K2, V> item = Tuple.Create(k1Item.Key, k2Item.Key, k2Item.Value);
                        yield return item;
                    }
                }
            }
        }


        private bool Remove<T1, T2>(Dictionary<T1, HashSet<T2>> toRemove,
            Dictionary<T1, List<KeyValuePair<T2, V>>> t1ValueStorage,
            Dictionary<T2, List<KeyValuePair<T1, V>>> t2ValueStorage,
            IEqualityComparer<T1> t1Comparer,
            Func<T1, T2, V, bool> predicate = null)
        {
            bool result = false;

            // Go via items which shall checked to be removed.
            foreach (KeyValuePair<T1, HashSet<T2>> groupToRemove in toRemove)
            {
                // Get all key2-values for the given key1.
                // Note: removing from the first dictionary.
                t1ValueStorage.TryGetValue(groupToRemove.Key, out List<KeyValuePair<T2, V>> t2Values);
                if (t2Values != null)
                {
                    // Go via all key2-value pairs.
                    for (int i = t2Values.Count - 1; i >= 0; --i)
                    {
                        KeyValuePair<T2, V> key2Value = t2Values[i];

                        // If key2-value could be removed.
                        if (groupToRemove.Value.Contains(key2Value.Key))
                        {
                            // Call predicate to find if it shall be removed.
                            bool removeFlag = predicate != null ? predicate(groupToRemove.Key, key2Value.Key, key2Value.Value) : true;
                            if (removeFlag)
                            {
                                t2Values.RemoveAt(i);

                                result = true;

                                // Get all key1-value pairs for the given key2.
                                // Note: it must be removed also from the second dictionary myK2Values.
                                t2ValueStorage.TryGetValue(key2Value.Key, out List<KeyValuePair<T1, V>> k1Values);
                                if (k1Values != null)
                                {
                                    for (int j = k1Values.Count - 1; j >= 0; --j)
                                    {
                                        KeyValuePair<T1, V> key1Value = k1Values[j];

                                        if (t1Comparer.Equals(groupToRemove.Key, key1Value.Key))
                                        {
                                            // If it is the same value as was removed from the first dictionary.
                                            if (myValueComparer.Equals(key2Value.Value, key1Value.Value))
                                            {
                                                k1Values.RemoveAt(j);
                                                break;
                                            }
                                        }
                                    }

                                    if (k1Values.Count == 0)
                                    {
                                        t2ValueStorage.Remove(key2Value.Key);
                                    }
                                }
                            }
                        }
                    }

                    if (t2Values.Count == 0)
                    {
                        t1ValueStorage.Remove(groupToRemove.Key);
                    }
                }
            }

            return result;
        }


        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
