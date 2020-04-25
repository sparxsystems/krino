using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Attributes
{
    /// <summary>
    /// Attributes indicating types of correlatives.
    /// </summary>
    public class Correlative : EnumGroupBase
    {
        public Correlative(EnumGroupBase parent) : base(parent)
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
