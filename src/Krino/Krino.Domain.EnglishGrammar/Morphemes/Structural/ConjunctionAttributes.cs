using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
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
            Starting = new EnumValue(this);
        }

        /// <summary>
        /// Connects equal phrases e.g. and, or, but.
        /// </summary>
        public EnumValue Coordinating { get; }

        /// <summary>
        /// Connects subordinated phrase to the main phrase e.g. until, because.
        /// </summary>
        public SubordinatingConjunctionAttribute Subordinating { get; }

        /// <summary>
        /// Conjuction which can be at the begnnig of the sentence e.g. And, But.
        /// </summary>
        public EnumValue Starting { get; }
    }
}
