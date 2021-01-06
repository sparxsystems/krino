using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.PatternAttributesArrangement
{
    /// <summary>
    /// Attributes indicating types of correlatives.
    /// </summary>
    public class CorrelativeAttributes : EnumGroupBase
    {
        public CorrelativeAttributes(EnumGroupBase parent) : base(parent)
        {
            Stative = new EnumValue(this);
            Adjunctive = new EnumValue(this);
            Circumstantial = new EnumValue(this);
        }

        /// <summary>
        /// Attribute for the correlative substituting the stative (O) grammar character.
        /// </summary>
        public EnumValue Stative { get; }

        /// <summary>
        /// Attribute for the correlative substituting the adjunctive (A) grammar character.
        /// </summary>
        public EnumValue Adjunctive { get; }

        /// <summary>
        /// Attribute for the correlative substituting the circumstantial (E) grammar character.
        /// </summary>
        public EnumValue Circumstantial { get; }
    }
}
