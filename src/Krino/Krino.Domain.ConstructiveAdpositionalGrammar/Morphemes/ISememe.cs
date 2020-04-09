using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public interface ISememe : ITreeBase<ISememe>
    {
        string Category { get; }
        string Value { get; }

        /// <summary>
        /// Parent.
        /// </summary>
        ISememe Hypernymy { get; }

        /// <summary>
        /// Children.
        /// </summary>
        IReadOnlyList<ISememe> Hyponymies { get; }
    }
}
