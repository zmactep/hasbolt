{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Criterion.Main
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Functor.Identity  (Identity (..))

import Database.Bolt               (Structure)
import Database.Bolt.Serialization (unpackAction, unpackT)

main :: IO ()
main = defaultMain
  [ bgroup "unpackAction"
    [ bench "unpack big node" $ nf
      (either (error "wat") id . runIdentity . unpackAction @Identity @Structure unpackT) input
    ]
  ]

input :: ByteString
input = either (error "wat") id $ B64.decode
  "sXGRs07JAf2Rh0JpZ05vZGWngWEqgWLJE4iBZACBZcFABb8Jlar3kINmb2/LAAAA6PJypSGBeMFACSH7TRLYSoR0ZXh00RhSTG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdC4gUGhhc2VsbHVzIGVnZXN0YXMgdGVtcHVzIHByZXRpdW0uIEluIGlkIHRyaXN0aXF1ZSBlcmF0LCBpZCBpbnRlcmR1bSBhdWd1ZS4gU2VkIG1hbGVzdWFkYSBtYXhpbXVzIGFyY3UsIHZlbCB1bHRyaWNpZXMgbGlndWxhIHRlbXBvciBmZXVnaWF0LiBVdCBlZ2V0IHRlbXB1cyB0dXJwaXMuIE1hdXJpcyBvcm5hcmUgdXQgbmliaCBhYyBzb2RhbGVzLiBPcmNpIHZhcml1cyBuYXRvcXVlIHBlbmF0aWJ1cyBldCBtYWduaXMgZGlzIHBhcnR1cmllbnQgbW9udGVzLCBuYXNjZXR1ciByaWRpY3VsdXMgbXVzLiBJbiBwb3J0dGl0b3IgZHVpIG1hc3NhLCB1dCBjb25kaW1lbnR1bSBlcm9zIGNvbmd1ZSBlZ2V0LiBQZWxsZW50ZXNxdWUgaGVuZHJlcml0IG5lcXVlIGxhY3VzLCBzZWQgdGVtcG9yIGVuaW0gbWF4aW11cyB2aXRhZS4gSW4gZmVsaXMgcXVhbSwgZmFjaWxpc2lzIGV1IG5pc2kgc2l0IGFtZXQsIGNvbW1vZG8gc2NlbGVyaXNxdWUgZG9sb3IuIENyYXMgYmliZW5kdW0gZXN0IGRvbG9yLCBzaXQgYW1ldCBhbGlxdWV0IGVyYXQgZWxlaWZlbmQgbm9uLiBOdWxsYW0gYWxpcXVhbSB0cmlzdGlxdWUgZG9sb3IgbmVjIHRpbmNpZHVudC4gU2VkIGF1Z3VlIHRlbGx1cywgZXVpc21vZCBzZWQgdGVtcG9yIGV1LCBwdWx2aW5hciBhdCBtYWduYS4gU2VkIGluIGF1Y3RvciBkdWkuIE51bGxhbSB2ZWwgdGVsbHVzIHBoYXJldHJhIG51bGxhIGZyaW5naWxsYSBpYWN1bGlzLiBQZWxsZW50ZXNxdWUgdmVsIGRvbG9yIG51bGxhLiBGdXNjZSBhY2N1bXNhbiBwbGFjZXJhdCBjb25ndWUuIE1vcmJpIG9ybmFyZSBmZWxpcyBleCwgdmVsIGNvbW1vZG8gbmVxdWUgcHVsdmluYXIgY29tbW9kby4gUHJhZXNlbnQgcXVpcyBsaWJlcm8gcXVpcyBmZWxpcyBjb25kaW1lbnR1bSBjb21tb2RvIGV1IG5vbiBkb2xvci4gUGVsbGVudGVzcXVlIHRlbXBvciB0ZWxsdXMgc2VkIGFsaXF1ZXQgaW1wZXJkaWV0LiBDbGFzcyBhcHRlbnQgdGFjaXRpIHNvY2lvc3F1IGFkIGxpdG9yYSB0b3JxdWVudCBwZXIgY29udWJpYSBub3N0cmEsIHBlciBpbmNlcHRvcyBoaW1lbmFlb3MuIERvbmVjIGFjIGNvbmRpbWVudHVtIHJpc3VzLiBEdWlzIHBvcnRhIGFudGUgaGVuZHJlcml0IGxvcmVtIGVnZXN0YXMsIHV0IHNjZWxlcmlzcXVlIG1pIGltcGVyZGlldC4gRnVzY2UgcG9ydGEgbWkgc2VkIGVyYXQgZGlnbmlzc2ltLCBjb25zZXF1YXQgZmF1Y2lidXMgbWV0dXMgdGluY2lkdW50LiBJbiBwb3J0dGl0b3IgbG9yZW0gbmVjIHRyaXN0aXF1ZSB0aW5jaWR1bnQuIE1hZWNlbmFzIGFsaXF1YW0gZG9sb3IgYXQgcGVsbGVudGVzcXVlIGltcGVyZGlldC4gQ3JhcyBibGFuZGl0IGVyb3MgcXVpcyBwbGFjZXJhdCBsYW9yZWV0LiBDdXJhYml0dXIgbm9uIGlhY3VsaXMgZWxpdC4gTWFlY2VuYXMgaWFjdWxpcyB0aW5jaWR1bnQgZGlhbSwgc2l0IGFtZXQgaW50ZXJkdW0gYW50ZSBjb25zZWN0ZXR1ciBuZWMuIE1vcmJpIGxvcmVtIG51bGxhLCBwb3J0YSBuZWMgaW1wZXJkaWV0IGEsIGxhb3JlZXQgaWQgbG9yZW0uIERvbmVjIGNvbmRpbWVudHVtIGFyY3UgYSBsYWNpbmlhIGNvbnNlY3RldHVyLiBDdXJhYml0dXIgdml0YWUgdml2ZXJyYSBsb3JlbSwgbm9uIGRhcGlidXMgbGliZXJvLiBBbGlxdWFtIHRpbmNpZHVudCBlbGVtZW50dW0gdGVsbHVzIHV0IGFjY3Vtc2FuLiBEb25lYyBzZWQgYWxpcXVhbSBqdXN0bywgZXQgcHVsdmluYXIgbGVjdHVzLiBWaXZhbXVzIGFjY3Vtc2FuIG5pc2wgYSBuZXF1ZSBhdWN0b3IsIGV1IHByZXRpdW0gbGFjdXMgZGlnbmlzc2ltLiBEb25lYyBlbGl0IGVzdCwgbWFsZXN1YWRhIHZpdGFlIHNhcGllbiBuZWMsIGRpY3R1bSB0cmlzdGlxdWUgdmVsaXQuIFN1c3BlbmRpc3NlIHRlbXB1cyBsb3JlbSBvZGlvLiBWZXN0aWJ1bHVtIHVsbGFtY29ycGVyIGxhY3VzIGV0IG1vbGVzdGllIGJpYmVuZHVtLiBTdXNwZW5kaXNzZSBwb3RlbnRpLiBTdXNwZW5kaXNzZSBzb2RhbGVzIG1hc3NhIGlkIHNlbSBldWlzbW9kIGxvYm9ydGlzIG5vbiBzaXQgYW1ldCBhdWd1ZS4gU3VzcGVuZGlzc2UgZXN0IGVzdCwgZGFwaWJ1cyB1bGxhbWNvcnBlciBuZXF1ZSBpbiwgbWFsZXN1YWRhIHZ1bHB1dGF0ZSBtYXVyaXMuIEZ1c2NlIGNvbnZhbGxpcyBwb3J0YSBkaWFtIG5vbiBibGFuZGl0LiBJbiBhdCB2dWxwdXRhdGUgZG9sb3IuIFNlZCBpbXBlcmRpZXQgZGljdHVtIHF1YW0gdml0YWUgY3Vyc3VzLiBNYWVjZW5hcyBmZWxpcyBhbnRlLCBwbGFjZXJhdCB2aXRhZSBpbnRlcmR1bSBwaGFyZXRyYSwgbWF0dGlzIG5vbiBzYXBpZW4uIE51bGxhbSBhYyB2ZWxpdCB0aW5jaWR1bnQsIGJsYW5kaXQgdGVsbHVzIHNpdCBhbWV0LCBzb2xsaWNpdHVkaW4gbGlndWxhLiBOdW5jIHZpdGFlIGVnZXN0YXMgbmlzbC4gSW50ZWdlciBmZXJtZW50dW0gdmVsIGVsaXQgaWQgcG9ydGEuIE51bmMgZXUgcG9ydGEgZXJhdC4gTWFlY2VuYXMgYWMgc2NlbGVyaXNxdWUgYXJjdS4gUHJhZXNlbnQgcHJldGl1bSBpbnRlcmR1bSBvcmNpIHZlbCBhY2N1bXNhbi4gU3VzcGVuZGlzc2UgZWdldCBudWxsYSBldSByaXN1cyBjb21tb2RvIGZldWdpYXQuIFByYWVzZW50IGZyaW5naWxsYSBlc3QgdmVsIGFudGUgbGFvcmVldCwgbmVjIGN1cnN1cyBtZXR1cyBzdXNjaXBpdC4gTWF1cmlzIGxhb3JlZXQgZXN0IGlkIHVybmEgZmVybWVudHVtIHBsYWNlcmF0LiBDcmFzIHZlc3RpYnVsdW0gbGVvIHNlZCBlbGl0IHVsbGFtY29ycGVyIGFjY3Vtc2FuIHZlbmVuYXRpcyBldSBtYXVyaXMuIE5hbSBkaWN0dW0gcHVsdmluYXIgZXN0LiBBZW5lYW4gdXQgZXJvcyBuZWMgZGlhbSBhbGlxdWFtIGNvbnZhbGxpcy4gUHJvaW4gZWxlaWZlbmQgbGlndWxhIHF1aXMgcGhhcmV0cmEgY29udmFsbGlzLiBDcmFzIGN1cnN1cywgYXVndWUgbm9uIHBvcnRhIG1vbGVzdGllLCBleCBtYXVyaXMgcG9zdWVyZSBsZWN0dXMsIHVsdHJpY2llcyBmYXVjaWJ1cyBvcmNpIHB1cnVzIGFjIGVuaW0uIE51bmMgYXQgbmVxdWUgdHJpc3RpcXVlLCBwZWxsZW50ZXNxdWUgYW50ZSBldCwgbW9sZXN0aWUgbmlzaS4gVXQgdWx0cmljZXMgYmliZW5kdW0gdm9sdXRwYXQuIEV0aWFtIGluIHVybmEgaW4gYXVndWUgaGVuZHJlcml0IGltcGVyZGlldCBzZWQgdXQgYXJjdS4gU2VkIGdyYXZpZGEgbmlzaSBzaXQgYW1ldCBmaW5pYnVzIHN1c2NpcGl0LiBOdW5jIG5lYyB0b3J0b3IgcXVpcyBvcmNpIGV1aXNtb2QgaW50ZXJkdW0uIE1hdXJpcyB2ZXN0aWJ1bHVtIGZlbGlzIGFjIG1hbGVzdWFkYSBldWlzbW9kLiBQcmFlc2VudCBhbGlxdWV0IG5lcXVlIHV0IGxpYmVybyBoZW5kcmVyaXQgc3VzY2lwaXQgbmVjIGV1IGxpYmVyby4gU2VkIHV0IGFudGUgc2VkIHVybmEgcG9ydHRpdG9yIGZhY2lsaXNpcy4gU3VzcGVuZGlzc2UgZGljdHVtIGF1Y3RvciB1bGxhbWNvcnBlci4gRG9uZWMgZWdldCBleCBhdCBuaWJoIGVnZXN0YXMgZWxlbWVudHVtLiBGdXNjZSBldSBqdXN0byBldWlzbW9kLCBzb2RhbGVzIG5pYmggZXQsIHRlbXB1cyBlbGl0LiBDcmFzIGluIGVuaW0gc2VtLiBVdCBwdWx2aW5hciBhdWd1ZSBldCBsaWd1bGEgaW50ZXJkdW0sIHNlZCB0ZW1wdXMgcmlzdXMgdmVzdGlidWx1bS4gTmFtIGN1cnN1cyBzZWQgYXVndWUgZXQgcG9ydHRpdG9yLiBTZWQgcmhvbmN1cyBpZCBmZWxpcyBuZWMgcGhhcmV0cmEuIFNlZCBmZXVnaWF0IHRpbmNpZHVudCBkdWksIGV1IHVsdHJpY2llcyBvcmNpIGltcGVyZGlldCBhYy4gVXQgYWMgYmxhbmRpdCB0b3J0b3IuIEZ1c2NlIGVmZmljaXR1ciwgZW5pbSBuZWMgZmF1Y2lidXMgdGVtcG9yLCB0dXJwaXMgbGVjdHVzIGRpZ25pc3NpbSBsZWN0dXMsIGEgY29uc2VxdWF0IG5pc2kgdmVsaXQgdml0YWUgZW5pbS4gUGhhc2VsbHVzIHRpbmNpZHVudCB1bHRyaWNpZXMgZGlhbSwgZXUgYXVjdG9yIGRvbG9yIHZhcml1cyBlZ2V0LiBTZWQgc2VkIGxpYmVybyB2dWxwdXRhdGUsIGRhcGlidXMgZG9sb3IgdXQsIHZ1bHB1dGF0ZSBqdXN0by4gVXQgZnJpbmdpbGxhIGVsaXQgYW50ZSwgc2VkIHNjZWxlcmlzcXVlIHR1cnBpcyBibGFuZGl0IHNpdCBhbWV0LiBQZWxsZW50ZXNxdWUgYXQgbmlzaSB2ZWxpdC4gRG9uZWMgY29uZGltZW50dW0gdmVsaXQgcXVpcyBkdWkgbW9sZXN0aWUgdWx0cmljZXMuIE5hbSB1dCBudW5jIHR1cnBpcy4gRHVpcyBsb2JvcnRpcyBuaWJoIGluIG1ldHVzIHZhcml1cyBkaWN0dW0uIEFsaXF1YW0gc2l0IGFtZXQgZW5pbSBldCBpcHN1bSBmcmluZ2lsbGEgY29tbW9kby4gUHJvaW4gcmhvbmN1cyBuZXF1ZSBzYXBpZW4sIGFjIGltcGVyZGlldCBhbnRlIHVsdHJpY2VzIHV0LiBVdCBuZWMgbmlzbCBwbGFjZXJhdCwgcHVsdmluYXIgbnVuYyB1dCwgb3JuYXJlIG5lcXVlLiBTZWQgZnJpbmdpbGxhIHZpdmVycmEgbWkgdmVsIGNvbW1vZG8uIEV0aWFtIGVmZmljaXR1ciBzaXQgYW1ldCBtaSBldCB0aW5jaWR1bnQuIFNlZCBtYXhpbXVzIGF1Z3VlIHNpdCBhbWV0IGxpZ3VsYSBzb2xsaWNpdHVkaW4gY29uc2VjdGV0dXIuIEludGVnZXIgZWxlbWVudHVtIHNhcGllbiBzaXQgYW1ldCBlbmltIHB1bHZpbmFyIHZlaGljdWxhLiBJbnRlZ2VyIGlkIGFudGUgc2VkIHNhcGllbiBwZWxsZW50ZXNxdWUgbG9ib3J0aXMuIFByYWVzZW50IHZpdGFlIGZhY2lsaXNpcyBmZWxpcywgcXVpcyB0aW5jaWR1bnQgbnVuYy4gRG9uZWMgaWQgcnV0cnVtIG9kaW8uIE51bGxhIHZpdGFlIHB1cnVzIGV1IGVuaW0gZGFwaWJ1cyBsb2JvcnRpcyBhIGlkIGRvbG9yLiBOdWxsYW0gdGluY2lkdW50IGV1IGFudGUgYSBhbGlxdWV0LiBQaGFzZWxsdXMgdm9sdXRwYXQgdXJuYSBuZWMgdmVsaXQgYmxhbmRpdCwgaW4gc29sbGljaXR1ZGluIGxpYmVybyB2b2x1dHBhdC4gTWFlY2VuYXMgc2NlbGVyaXNxdWUgYWxpcXVhbSBlcmF0IGVnZXQgbWF4aW11cy4gRnVzY2UgY3Vyc3VzIHBoYXJldHJhIGRpZ25pc3NpbS4gVXQgYWxpcXVhbSwgcHVydXMgYSBhdWN0b3IgcG9ydHRpdG9yLCBlcm9zIHRlbGx1cyBtYWxlc3VhZGEgZXN0LCBxdWlzIGhlbmRyZXJpdCBkaWFtIHR1cnBpcyB1dCBleC4gQ2xhc3MgYXB0ZW50IHRhY2l0aSBzb2Npb3NxdSBhZCBsaXRvcmEgdG9ycXVlbnQgcGVyIGNvbnViaWEgbm9zdHJhLCBwZXIgaW5jZXB0b3MgaGltZW5hZW9zLiBRdWlzcXVlIGZhdWNpYnVzLCB0b3J0b3IgYSBwaGFyZXRyYSBzdXNjaXBpdCwgaXBzdW0gYXVndWUgaGVuZHJlcml0IG5lcXVlLCBldCBhdWN0b3IgbmlzbCBmZWxpcyBxdWlzIGR1aS4gUXVpc3F1ZSByaG9uY3VzIGR1aSBhdCBwbGFjZXJhdCBzb2xsaWNpdHVkaW4uIE1hdXJpcyBydXRydW0gbHVjdHVzIHR1cnBpcy4gUXVpc3F1ZSBzY2VsZXJpc3F1ZSBsYWN1cyBtYXVyaXMsIHF1aXMgbWF4aW11cyBwdXJ1cyBzY2VsZXJpc3F1ZSBub24uIFV0IGV0IGVzdCBzaXQgYW1ldCBxdWFtIGxhb3JlZXQgcHJldGl1bSBpbiBhdCB0b3J0b3IuIE51bGxhbSBhcmN1IGRpYW0sIGF1Y3RvciBiaWJlbmR1bSBsZWN0dXMgZXQsIGJsYW5kaXQgbGFjaW5pYSBxdWFtLiBBbGlxdWFtIGFsaXF1YW0gcG9zdWVyZSBuaXNsIGF0IGFjY3Vtc2FuLiBQaGFzZWxsdXMgYSBwb3N1ZXJlIGRpYW0sIG5lYyBiaWJlbmR1bSBzZW0uIFN1c3BlbmRpc3NlIGludGVyZHVtIG1hc3NhIGVsaXQsIHF1aXMgZmV1Z2lhdCBuaXNpIHZlaGljdWxhIHZpdGFlLiBTZWQgZHVpIG1hdXJpcywgZWxlbWVudHVtIGV0IGFudGUgcXVpcywgdnVscHV0YXRlIGVmZmljaXR1ciBsaWJlcm8uIE5hbSBhbGlxdWV0IGVsaXQgYWMgbGliZXJvIGFsaXF1ZXQsIGVnZXQgZmluaWJ1cyBuaXNpIHRpbmNpZHVudC4gU3VzcGVuZGlzc2UgdmVsIG5pc2wgaW4gdGVsbHVzIHBoYXJldHJhIGdyYXZpZGEgZXQgdml0YWUgbWF1cmlzLiBDdXJhYml0dXIgcG9zdWVyZSBjb252YWxsaXMgYWxpcXVldC4gTnVsbGFtIHNlZCBlc3QgYXQgbmlzbCBhdWN0b3IgbGFjaW5pYSBpZCBzaXQgYW1ldCBtYWduYS4gTnVsbGEgZXQgdG9ydG9yIHR1cnBpcy4gU3VzcGVuZGlzc2Ugc2FwaWVuIGVyb3MsIGZlcm1lbnR1bSBxdWlzIHBlbGxlbnRlc3F1ZSB2ZWwsIGRpZ25pc3NpbSB2ZWwgdXJuYS4gQWxpcXVhbSBuaXNpIG1hdXJpcywgc2NlbGVyaXNxdWUgZWdldCBuaWJoIHF1aXMsIGNvbnNlY3RldHVyIGZlcm1lbnR1bSBqdXN0by4gUHJhZXNlbnQgZWdldCBlbGVtZW50dW0gaXBzdW0uIFNlZCBuaWJoIG1pLCB0aW5jaWR1bnQgcXVpcyBuaWJoIHV0LCB1bHRyaWNpZXMgZmFjaWxpc2lzIG5lcXVlLg=="