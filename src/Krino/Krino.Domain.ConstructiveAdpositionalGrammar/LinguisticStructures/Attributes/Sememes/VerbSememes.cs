using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// Semantic attributes of verbs.
    /// </summary>
    public class VerbSememes : EnumGroupBase
    {
        public VerbSememes(EnumGroupBase parent) : base(parent)
        {
            Person = new PersonSememes(this);
            Number = new NumberSememes(this);
            Tense = new VerbTenseSememes(this);
            Aspect = new VerbAspectSememes(this);
            Mood = new VerbMoodSememes(this);
            Voice = new VerbVoiceSememes(this);
        }

        public PersonSememes Person { get; }

        public NumberSememes Number { get; }

        /// <summary>
        /// Refers to when the action occurred.
        /// </summary>
        public VerbTenseSememes Tense { get; }

        /// <summary>
        /// Refers to the flow of time.
        /// </summary>
        public VerbAspectSememes Aspect { get; }

        /// <summary>
        /// Refers to the attitude of the action.
        /// </summary>
        public VerbMoodSememes Mood { get; }

        /// <summary>
        /// Refers who does the verb (active vs passive).
        /// </summary>
        public VerbVoiceSememes Voice { get; }
    }
}
