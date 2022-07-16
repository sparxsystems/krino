using System;

namespace Krino.Vertical.Utils.Transformations
{
    public class DropTransformation<T> : ITransformation<T>
    {
        private Func<T, int, int, T> myOnDrop;

        public DropTransformation(int idx, int count, Func<T, int, int, T> onDrop)
        {
            StartIdx = idx;
            Count = count;
            myOnDrop = onDrop;
        }

        public int StartIdx { get; private set; }
        public int Count { get; private set; }

        public T Transform(T value) => myOnDrop(value, StartIdx, Count);


        public bool Equals(ITransformation<T> other) =>
            other is DropTransformation<T> otherTransfromation &&
            StartIdx == otherTransfromation.StartIdx &&
            Count == otherTransfromation.Count &&
            myOnDrop == otherTransfromation.myOnDrop;

    }
}
