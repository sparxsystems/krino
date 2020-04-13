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
        /// The morpheme specified by the pattern.
        /// </summary>
        IMorpheme Morpheme { get; }

        /// <summary>
        /// The valency position the element shall saturates.
        /// </summary>
        /// <remarks>
        /// Note: According to 'Constructive Adpositional Grammar' page 23 the valency is a feature of the governor and not
        /// necessarily restricted to verbal entities.<br/>
        /// <br/>
        /// If the value is 0 the valency position is not set.
        /// </remarks>
        int ValencyPosition { get; }

        /// <summary>
        /// True if the element represents a correlative adposition.
        /// </summary>
        /// <remarks>
        /// Correlatives are words which transform a secondary phrase or even a sentence into a stative, adjunctive or circumstantial of the main phrase.
        /// </remarks>
        bool IsCorrelativeAdposition { get; }

        /// <summary>
        /// True if the pattern represents an element which is substituted by the correlative adposition.
        /// </summary>
        bool IsCorrelativeSubstitute { get; }

        /// <summary>
        /// True if the textual phrase has reversed order, i.e. Left-Right.
        /// </summary>
        bool IsReversed { get; }

        /// <summary>
        /// The rule which specifies which adpositions fit this pattern.
        /// </summary>
        IRule<IPattern> AdPosition { get; }

        /// <summary>
        /// The rule which specifies which right children fit this pattern.
        /// </summary>
        IRule<IPattern> Right { get; }

        /// <summary>
        /// The rule which specifies which left children fit this pattern.
        /// </summary>
        IRule<IPattern> Left { get; }
    }
}
