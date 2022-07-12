using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Transformations
{
    public class PrependTransformation<T> : ITransformation<T>
    {
        private IEqualityComparer<T> myComparer = EqualityComparer<T>.Default;
        private Func<T, T, T> myOnPrepend;

        public PrependTransformation(T toPrepend, Func<T, T, T> onPrepend)
        {
            ToPrepend = toPrepend;
            myOnPrepend = onPrepend;
        }

        public T ToPrepend { get; private set; }

        public T Transform(T value) => myOnPrepend(ToPrepend, value);

        public bool Equals(ITransformation<T> other) =>
            other is PrependTransformation<T> otherTransformation &&
            myComparer.Equals(ToPrepend, otherTransformation.ToPrepend) &&
            ToPrepend.Equals(otherTransformation.ToPrepend);
    }
}
