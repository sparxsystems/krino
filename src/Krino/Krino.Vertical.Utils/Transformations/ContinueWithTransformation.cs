namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Concatenates transformations.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class ContinueWithTransformation<T> : ITransformation<T>
    {
        private ITransformation<T> myTransformation1;
        private ITransformation<T> myTransformation2;

        public ContinueWithTransformation(ITransformation<T> transformation1, ITransformation<T> transformation2)
        {
            myTransformation1 = transformation1;
            myTransformation2 = transformation2;
        }

        /// <summary>
        /// Performs both transformations.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public T Transform(T value)
        {
            T result = myTransformation1.Transform(value);
            result = myTransformation2.Transform(result);
            return result;
        }

        public bool Equals(ITransformation<T> other) =>
            other is ContinueWithTransformation<T> otheTransformation &&
            myTransformation1.Equals(otheTransformation.myTransformation1) &&
            myTransformation2.Equals(otheTransformation.myTransformation2);
    }
}
