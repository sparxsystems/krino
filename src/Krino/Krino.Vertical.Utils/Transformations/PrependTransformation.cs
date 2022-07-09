using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Transformations
{
    public class PrependTransformation<T, TI> : ITransformation<T>
    {
        private IEqualityComparer<TI> myComparer = EqualityComparer<TI>.Default;
        private Func<T, TI, T> myOnPrepend;

        public PrependTransformation(TI toPrepend, Func<T, TI, T> onPrepend)
        {
            ToPrepend = toPrepend;
            myOnPrepend = onPrepend;
        }

        public TI ToPrepend { get; private set; }

        public T Transform(T value) => myOnPrepend(value, ToPrepend);

        public bool Equals(ITransformation<T> other) =>
            other is PrependTransformation<T, TI> otherTransformation &&
            myComparer.Equals(ToPrepend, otherTransformation.ToPrepend) &&
            ToPrepend.Equals(otherTransformation.ToPrepend);
    }
}
