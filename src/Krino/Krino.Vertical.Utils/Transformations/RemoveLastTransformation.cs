using System;

namespace Krino.Vertical.Utils.Transformations
{
    public class RemoveLastTransformation<T, TI> : ITransformation<T>
    {
        private Func<T, T> myOnRemove;

        public RemoveLastTransformation(Func<T, T> onRemove)
        {
            myOnRemove = onRemove;
        }

        public T Transform(T value) => myOnRemove(value);

        public bool Equals(ITransformation<T> other) =>
            other is RemoveLastTransformation<T, TI>;
    }
}
