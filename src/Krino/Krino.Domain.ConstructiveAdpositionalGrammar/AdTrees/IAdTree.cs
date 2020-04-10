using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Declares the AdTree as defined in the document Constructive Adposition Grammar.
    /// </summary>
    public interface IAdTree : IEnumerable<IAdTree>
    {
        /// <summary>
        /// Type of the lement in the tree.
        /// </summary>
        /// <remarks>
        /// If references to Governor and Dependent exist then it is AdPosition,
        /// and if it is not on the root then it is also either Governor or Dependent.
        /// </remarks>
        AdTreeElementType ElementType { get; }

        /// <summary>
        /// The relation between the governor and the dependent.
        /// </summary>
        RelationType Relation { get; }

        /// <summary>
        /// Morpheme.
        /// </summary>
        IMorpheme Morpheme { get; set; }

        /// <summary>
        /// If adposition then it returns the grammar character inherited from the governor.
        /// If not adposition then it returns the own grammar character.
        /// </summary>
        GrammarCharacter RaisedGrammarCharacter { get; }

        /// <summary>
        /// Reference to the AdPosition (direct parent).
        /// </summary>
        IAdTree AdPosition { get; set; }

        /// <summary>
        /// Sequence of parents to the root.
        /// </summary>
        IEnumerable<IAdTree> AdPositions { get; }

        /// <summary>
        /// The governor (the righ child).
        /// </summary>
        IAdTree Governor { get; set; }

        /// <summary>
        /// Sequence of all governors down the tree.
        /// </summary>
        IEnumerable<IAdTree> Governors { get; }

        /// <summary>
        /// the dependent (the left child).
        /// </summary>
        IAdTree Dependent { get; set; }

        /// <summary>
        /// Sibling (sibling governor or sibling dependent).
        /// </summary>
        IAdTree Sibling { get; }

        /// <summary>
        /// If it is a stative or a raised stative then it returns the valency it saturates. Otherwise it returns 0.
        /// </summary>
        int SaturatedValency { get; }

        /// <summary>
        /// If this is a verband then it returns the list of dependents which saturate particular valencies.
        /// </summary>
        IReadOnlyList<IAdTree> SaturatedValencies { get; }


        /// <summary>
        /// Returns the phrase ordered sequence of verbants.
        /// </summary>
        IEnumerable<IAdTree> Verbants { get; }

        /// <summary>
        /// Returns the sequence of the phrase which is represented by the adtree.
        /// </summary>
        IEnumerable<IAdTree> Phrase { get; }
    }
}
