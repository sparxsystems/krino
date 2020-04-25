using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    /// <summary>
    /// Declares the pattern which gives the form to the linguistic construction.
    /// </summary>
    public interface IPattern
    {
        /// <summary>
        /// The morpheme specified by the pattern.
        /// </summary>
        IMorpheme Morpheme { get; }

        /// <summary>
        /// Attributes of the pattern.
        /// </summary>
        ulong PatternAttributes { get; }


        IPattern RequiredAdPosition { get; }

        IPattern RequiredLeft { get; }

        IPattern RequiredRight { get; }

        IPattern RequiredGovernor { get; }

        int ValencyPosition { get; }
    }
}
