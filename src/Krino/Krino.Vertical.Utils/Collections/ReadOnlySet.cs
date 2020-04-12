using System;
using System.Collections;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Readonly wrapper for HashSet.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class ReadOnlySet<T> : ISet<T>, IReadOnlyCollection<T>
    {
        private HashSet<T> myHashSet;

        public ReadOnlySet(HashSet<T> hashSet)
        {
            myHashSet = hashSet;
        }

        public int Count { get { return myHashSet.Count; } }

        public bool IsReadOnly { get { return true; } }

        public bool Add(T item) { throw new NotSupportedException("Add is not supported in readonly."); }

        public void Clear() { throw new NotSupportedException("Clear is not supported in readonly."); }

        public bool Contains(T item)
        {
            return myHashSet.Contains(item);
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            myHashSet.CopyTo(array, arrayIndex);
        }

        public void ExceptWith(IEnumerable<T> other) { throw new NotSupportedException("ExceptWith is not supported in readonly."); }

        public IEnumerator<T> GetEnumerator()
        {
            return myHashSet.GetEnumerator();
        }

        public void IntersectWith(IEnumerable<T> other) { throw new NotSupportedException("IntersectWith is not supported in readonly."); }


        public bool IsProperSubsetOf(IEnumerable<T> other)
        {
            return myHashSet.IsProperSubsetOf(other);
        }

        public bool IsProperSupersetOf(IEnumerable<T> other)
        {
            return myHashSet.IsProperSupersetOf(other);
        }

        public bool IsSubsetOf(IEnumerable<T> other)
        {
            return myHashSet.IsSubsetOf(other);
        }

        public bool IsSupersetOf(IEnumerable<T> other)
        {
            return myHashSet.IsSupersetOf(other);
        }

        public bool Overlaps(IEnumerable<T> other)
        {
            return myHashSet.Overlaps(other);
        }

        public bool Remove(T item) { throw new NotSupportedException("Remove is not supported in readonly."); }


        public bool SetEquals(IEnumerable<T> other)
        {
            return myHashSet.SetEquals(other);
        }

        public void SymmetricExceptWith(IEnumerable<T> other) { throw new NotSupportedException("SymmetricExceptWith is not supported in readonly."); }


        public void UnionWith(IEnumerable<T> other) { throw new NotSupportedException("UnionWith is not supported in readonly."); }


        void ICollection<T>.Add(T item)
        {
            Add(item);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

}
