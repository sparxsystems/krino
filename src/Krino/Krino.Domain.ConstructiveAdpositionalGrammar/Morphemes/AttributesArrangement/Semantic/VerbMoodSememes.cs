using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Semantic
{
    /// <summary>
    /// Attributes of mood.
    /// </summary>
    public class VerbMoodSememes : EnumGroupBase
    {
        public VerbMoodSememes(EnumGroupBase parent) : base(parent)
        {
            Indicative = new EnumValue(this);
            Imperative = new EnumValue(this);
            Subjunctive = new EnumValue(this);
        }

        /// <summary>
        /// Expresses a simple statement of fact.
        /// </summary>
        public EnumValue Indicative { get; }

        /// <summary>
        /// Expresses a command.
        /// </summary>
        public EnumValue Imperative { get; }

        /// <summary>
        /// Expresses a desire.
        /// </summary>
        public EnumValue Subjunctive { get; }
    }
}
