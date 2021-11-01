using System.Runtime.Serialization;

namespace Krino.Prolog.Client
{
    [DataContract]
    public class EvaluationResult
    {
        [DataMember(Name = "result")]
        public bool Result { get; set; }
    }
}
