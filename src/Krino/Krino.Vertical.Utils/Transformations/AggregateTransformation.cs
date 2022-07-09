using System.Linq;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Concatenates transformations.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class AggregateTransformation<T> : ITransformation<T>
    {
        private ITransformation<T>[] myTransformations;

        public AggregateTransformation(params ITransformation<T>[] transformations)
        {
            myTransformations = transformations;
        }

        /// <summary>
        /// Performs both transformations.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public T Transform(T value)
        {
            T result;

            if (myTransformations != null && myTransformations.Any())
            {
                result = myTransformations.First().Transform(value);

                if (myTransformations.Skip(1).Any())
                {
                    result = myTransformations.Skip(1).Aggregate(result, (x, t) => t.Transform(x));
                }
            }
            else
            {
                result = default(T);
            }

            return result;
        }

        public bool Equals(ITransformation<T> other) =>
            other is AggregateTransformation<T> otherTransformation &&
            myTransformations.SequenceEqual(otherTransformation.myTransformations);
    }
}
