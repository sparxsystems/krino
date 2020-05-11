using System;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Declares the transformation.
    /// </summary>
    /// <typeparam name="T">Type of the value which shall be transformed.</typeparam>
    public interface ITransformation<T> : IEquatable<ITransformation<T>>
    {
        /// <summary>
        /// Transform
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        T Transform(T value);
    }
}
