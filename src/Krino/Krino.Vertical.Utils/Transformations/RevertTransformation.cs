namespace Krino.Vertical.Utils.Transformations
{
    public abstract class RevertTransformation<T> : ITransformation<T>
    {
        public RevertTransformation(ITransformation<T> toRevert)
        {
            ToRevert = toRevert;
        }

        public ITransformation<T> ToRevert { get; private set; }

        public abstract T Transform(T value);

        public bool Equals(ITransformation<T> other) =>
            GetType() == other.GetType() &&
            other is RevertTransformation<T> otherTransformation &&
            ToRevert.Equals(otherTransformation.ToRevert);
    }
}
