using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    /// <summary>
    /// Decares the sememe, the unit of meaning carried by a morpheme.
    /// </summary>
    public interface ISememe : ITreeBase<ISememe>
    {
        /// <summary>
        /// The meaning the sememe carries. E.g. tense
        /// </summary>
        string Meaning { get; }

        /// <summary>
        /// If applicable the value of the sememe. E.g. present
        /// </summary>
        string Value { get; }

        /// <summary>
        /// Parent sememe.
        /// </summary>
        ISememe Hypernymy { get; }

        /// <summary>
        /// Children sememes.
        /// </summary>
        IReadOnlyList<ISememe> Hyponymies { get; }
    }
}
