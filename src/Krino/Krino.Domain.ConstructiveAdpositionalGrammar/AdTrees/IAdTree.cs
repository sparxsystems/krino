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
        /// Information prominance trajectory.
        /// </summary>
        Prominence InformationProminence { get; set; }

        /// <summary>
        /// Morpheme.
        /// </summary>
        Morpheme Morpheme { get; set; }

        /// <summary>
        /// Reference to the AdPosition (direct parent).
        /// </summary>
        IAdTree AdPosition { get; set; }

        /// <summary>
        /// Sequence of parents to the root.
        /// </summary>
        IEnumerable<IAdTree> AdPositions { get; }

        /// <summary>
        /// Reference to the governort (the righ branch).
        /// </summary>
        IAdTree Governor { get; set; }

        /// <summary>
        /// Reference to the dependent (the left branch).
        /// </summary>
        IAdTree Dependent { get; set; }
    }
}
