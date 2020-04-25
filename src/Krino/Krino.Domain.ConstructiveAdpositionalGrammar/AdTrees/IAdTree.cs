using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Declares the AdTree as defined in the document Constructive Adposition Grammar.
    /// </summary>
    public interface IAdTree : IEnumerable<IAdTree>
    {
        /// <summary>
        /// Pattern followed by the adtree element.
        /// </summary>
        IPattern Pattern { get; set; }

        /// <summary>
        /// Morpheme.
        /// </summary>
        IMorpheme Morpheme { get; set; }

        /// <summary>
        /// The grammar character of this tree element.
        /// </summary>
        GrammarCharacter GrammarCharacter { get; }

        /// <summary>
        /// Grammar character inherited from the governor. If this is the governor then it returns its grammar character.
        /// </summary>
        /// <remarks>
        /// If there is no a child governor then it returns Epsilon.
        /// </remarks>
        GrammarCharacter InheritedGrammarCharacter { get; }

        /// <summary>
        /// Reference to the AdPosition (direct parent). Null if it is the root.
        /// </summary>
        IAdTree AdPosition { get; set; }

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
        /// The righ child.
        /// </summary>
        IAdTree Right { get; set; }

        /// <summary>
        /// The sequence of elements on the right branch.
        /// </summary>
        IEnumerable<IAdTree> RightChildren { get; }

        /// <summary>
        /// The left child.
        /// </summary>
        IAdTree Left { get; set; }

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
        /// If this is the governer then it returns the governor of this governor.
        /// </remarks>
        IAdTree Governor { get; }

        /// <summary>
        /// Returns true if this tree element is the governor.
        /// </summary>
        /// <remarks>
        /// The element is the governor if it is located on right and its grammar character is not epsilon.
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
        /// Returns the first parent adposition element which has set the valency position for this element.
        /// </summary>
        /// <remarks>
        /// If this element has set the valency attribute then it finds the valencyAdPosition of this 'valencied' element.
        /// </remarks>
        IAdTree ValencyAdPosition { get; }


        /// <summary>
        /// It returns the sequence of dependents which saturate particular valencies.
        /// </summary>
        IEnumerable<IAdTree> ValencyAdPositions { get; }


        /// <summary>
        /// Returns the phrase ordered sequence of verbants.
        /// </summary>
        IEnumerable<IAdTree> Verbants { get; }

        /// <summary>
        /// Returns the phrase sequence of elements.
        /// </summary>
        /// <remarks>
        /// Note: The method is async to avoid recursive calls.
        /// </remarks>
        Task<IEnumerable<IAdTree>> GetPhraseElementsAsync();

        /// <summary>
        /// Returns the phrase text represented by this adtree.
        /// </summary>
        string Phrase { get; }


    }
}
