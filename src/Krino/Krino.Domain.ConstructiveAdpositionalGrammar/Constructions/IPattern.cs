using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Patterns;
using System;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    /// <summary>
    /// Declares the pattern which gives the form to the linguistic construction.
    /// </summary>
    public interface IPattern : IEquatable<IPattern>
    {
        /// <summary>
        /// The pattern specified morpheme.
        /// </summary>
        IMorpheme Morpheme { get; }

        /// <summary>
        /// The valency position the element saturates.
        /// </summary>
        /// <remarks>
        /// According to 'Constructive Adpositional Grammar' page 23 the valency is a feature of the governor and not
        /// necessarily restricted to verbal entities.<br/>
        /// <br/>
        /// If the value is 0 the valency position is not set.
        /// </remarks>
        int ValencyPosition { get; }

        /// <summary>
        /// True if the order RightChild-LeftChild is reversed in the phrase.
        /// </summary>
        bool IsReversed { get; }

        /// <summary>
        /// The rule for fitting parent adposition.
        /// </summary>
        IRule<IPattern> AdPosition { get; }

        /// <summary>
        /// The rule for fitting right child.
        /// </summary>
        IRule<IPattern> RightChild { get; }

        /// <summary>
        /// The rule for fitting left child.
        /// </summary>
        IRule<IPattern> LeftChild { get; }
    }
}
