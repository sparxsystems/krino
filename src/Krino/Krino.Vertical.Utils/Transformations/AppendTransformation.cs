using System;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Transformations
{
    public class AppendTransformation<T> : ITransformation<T>
    {
        private IEqualityComparer<T> myComparer = EqualityComparer<T>.Default;
        private Func<T, T, T> myOnAppend;

        public AppendTransformation(T toAppend, Func<T, T, T> onAppend)
        {
            ToAppend = toAppend;
            myOnAppend = onAppend;
        }

        public T ToAppend { get; private set; }

        public T Transform(T value) => myOnAppend(value, ToAppend);

        public bool Equals(ITransformation<T> other) =>
            other is AppendTransformation<T> otherTransformation &&
            myComparer.Equals(ToAppend, otherTransformation.ToAppend) &&
            ToAppend.Equals(otherTransformation.ToAppend);
    }
}
