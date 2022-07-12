using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Replaces the incoming value by the new value.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class ReplaceTransformation<T> : ITransformation<T>
    {
        private IEqualityComparer<T> myComparer = EqualityComparer<T>.Default;
        private Func<T, T, T, T> myOnReplace;

        public ReplaceTransformation(T oldValue, T newValue, Func<T, T, T, T> onReplace)
        {
            OldValue = oldValue;
            NewValue = newValue;
            myOnReplace = onReplace;
        }

        public T OldValue { get; private set; }

        public T NewValue { get; private set; }

        public T Transform(T value) => myOnReplace(value, OldValue, NewValue);


        public bool Equals(ITransformation<T> other) =>
            other is ReplaceTransformation<T> otherTransfromation &&
            myComparer.Equals(OldValue, otherTransfromation.OldValue) &&
            myComparer.Equals(NewValue, otherTransfromation.NewValue);
        
    }
}
