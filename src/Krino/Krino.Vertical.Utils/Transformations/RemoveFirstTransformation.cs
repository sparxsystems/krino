using System;

namespace Krino.Vertical.Utils.Transformations
{
    public class RemoveFirstTransformation<T, TI> : ITransformation<T>
    {
        private Func<T, T> myOnRemove;

        public RemoveFirstTransformation(Func<T, T> onRemove)
        {
            myOnRemove = onRemove;
        }

        public T Transform(T value) => myOnRemove(value);

        public bool Equals(ITransformation<T> other) =>
            other is RemoveFirstTransformation<T, TI>;
    }
}
