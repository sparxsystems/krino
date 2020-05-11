using System.Collections.Generic;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Replaces the incoming value by the new value.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class ReplaceWithTransformation<T> : ITransformation<T>
    {
        private IEqualityComparer<T> myComparer = EqualityComparer<T>.Default;

        public ReplaceWithTransformation(T newValue)
        {
            NewValue = newValue;
        }

        /// <summary>
        /// Value which is returned by the transformation.
        /// </summary>
        public T NewValue { get; private set; }

        /// <summary>
        /// Replaces the value by the new value.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public T Transform(T value) => NewValue;


        public bool Equals(ITransformation<T> other) =>
            other is ReplaceWithTransformation<T> otherTransfromation &&
            myComparer.Equals(NewValue, otherTransfromation.NewValue);
        
    }
}
