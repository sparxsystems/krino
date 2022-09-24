using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Conjunction attributes.
    /// </summary>
    public class ConjunctionAttributes : EnumGroupBase
    {
        public ConjunctionAttributes(EnumGroupBase parent) : base(parent)
        {
            Coordinating = new EnumValue(this);
            Subordinating = new SubordinatingConjunctionAttribute(this);
            Correlative = new EnumValue(this);
        }

        /// <summary>
        /// Connect items that are grammatically equal.
        /// </summary>
        /// <remarks>
        /// The seven coordinating conjunctions used as connecting words at the beginning of
        /// an independent clause are and, but, for, or, nor, so, and yet. 
        /// </remarks>
        public EnumValue Coordinating { get; }

        /// <summary>
        /// Connects subordinated phrase to the main phrase e.g. until, because.
        /// </summary>
        /// <remarks>
        /// Some common subordinating conjunctions are: after, as, before, once, since, until, and while.
        /// </remarks>
        public SubordinatingConjunctionAttribute Subordinating { get; }



        /// <summary>
        /// This type of conjunction always comes in a pair and is used to join grammatically equal elements in a sentence.
        /// </summary>
        /// <remarks>
        /// Common pairs include either … or, neither … nor, not only … but also, and both … and. In most cases, no comma should be used between the two elements.
        /// </remarks>
        public EnumValue Correlative { get; }
    }
}
