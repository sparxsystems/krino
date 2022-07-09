using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Transformations
{
    public class AppendTransformation<T, TI> : ITransformation<T>
    {
        private IEqualityComparer<TI> myComparer = EqualityComparer<TI>.Default;
        private Func<T, TI, T> myOnAppend;

        public AppendTransformation(TI toAppend, Func<T, TI, T> onAppend)
        {
            ToAppend = toAppend;
            myOnAppend = onAppend;
        }

        public TI ToAppend { get; private set; }

        public T Transform(T value) => myOnAppend(value, ToAppend);

        public bool Equals(ITransformation<T> other) =>
            other is AppendTransformation<T, TI> otherTransformation &&
            myComparer.Equals(ToAppend, otherTransformation.ToAppend) &&
            ToAppend.Equals(otherTransformation.ToAppend);
    }
}
