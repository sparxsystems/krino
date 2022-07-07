using Krino.Domain.ConstructiveGrammar.LinguisticStructures;
using System;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveGrammar.AdTrees
{
    /// <summary>
    /// Declares the AdTree as defined in the document Constructive Adposition Grammar.
    /// </summary>
    public interface IAdTree : IEquatable<IAdTree>, IEnumerable<IAdTree>
    {
        /// <summary>
        /// Morpheme.
        /// </summary>
        IMorpheme Morpheme { get; }

        /// <summary>
        /// Reference to the AdPosition (direct parent). Null if it is the root.
        /// </summary>
        IAdTree AdPosition { get; set; }

        /// <summary>
        /// The righ child.
        /// </summary>
        IAdTree Right { get; set; }

        /// <summary>
        /// The left child.
        /// </summary>
        IAdTree Left { get; set; }

        /// <summary>
        /// Returns true if the text in the left branch is before the text in the right branch.
        /// </summary>
        bool IsLeftFirst { get; set; }

        /// <summary>
        /// Returns true if this tree element is the adpostion.
        /// </summary>
        /// <remarks>
        /// It is the adposition if the LeftChild or the RightChild is not null.
        /// </remarks>
        bool IsAdPosition { get; }

        /// <summary>
        /// Sequence of parents to the root.
        /// </summary>
        IEnumerable<IAdTree> AdPositions { get; }

        /// <summary>
        /// Returns the root of the adtree.
        /// </summary>
        IAdTree Root { get; }

        /// <summary>
        /// The sequence of elements on the right branch.
        /// </summary>
        IEnumerable<IAdTree> RightChildren { get; }

        /// <summary>
        /// Returns true if this tree element is located on the right branch.
        /// </summary>
        bool IsOnRight { get; }

        /// <summary>
        /// Returns true if this tree element is located on the left branch.
        /// </summary>
        bool IsOnLeft { get; }

        /// <summary>
        /// Returns the governor which controls this tree element.
        /// </summary>
        /// <remarks>
        /// If the governor does not exist then it returns null.
        /// </remarks>
        /// <returns></returns>
        IAdTree GetMyGovernor();

        /// <summary>
        /// Returns true if this tree element is the governor.
        /// </summary>
        /// <remarks>
        /// The element is the governor if it is located on the right and it acts like a morpheme.
        /// It is the element which may have several dependents.
        /// </remarks>
        bool IsGovernor { get; }

        /// <summary>
        /// If this is the governor then it returns adpositions containing its dependents otherwise empty sequence.
        /// </summary>
        IEnumerable<IAdTree> DependentAdPositions { get; }

        /// <summary>
        /// Returns true if this tree element is dependent.
        /// </summary>
        /// <remarks>
        /// The element is dependent if it is located on left and its grammar character is not U.
        /// In contrary to IsGovernor its grammar character can be epsilon.
        /// </remarks>
        bool IsDependent { get; }

        /// <summary>
        /// Returns sequence of adtrees in the adtree syntax order. (right branch is always on right)
        /// </summary>
        /// <returns></returns>
        IEnumerable<IAdTree> GetAdTreesInAdTreeOrder();


        /// <summary>
        /// Returns the text phrase represented by this adtree.
        /// </summary>
        string Phrase { get; }
    }
}
