using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Replaces the incoming value by the new value.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class ReplaceTransformation<T, TI> : ITransformation<T>
    {
        private IEqualityComparer<TI> myComparer = EqualityComparer<TI>.Default;
        private Func<T, TI, TI, T> myOnReplace;

        public ReplaceTransformation(TI oldValue, TI newValue, Func<T, TI, TI, T> onReplace)
        {
            OldValue = oldValue;
            NewValue = newValue;
            myOnReplace = onReplace;
        }

        public TI OldValue { get; private set; }

        public TI NewValue { get; private set; }

        public T Transform(T value) => myOnReplace(value, OldValue, NewValue);


        public bool Equals(ITransformation<T> other) =>
            other is ReplaceTransformation<T, TI> otherTransfromation &&
            myComparer.Equals(OldValue, otherTransfromation.OldValue) &&
            myComparer.Equals(NewValue, otherTransfromation.NewValue);
        
    }
}
