module AuthUtil

open System
open System.Text
open Saturn
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt
open Microsoft.IdentityModel.Tokens
open Microsoft.AspNetCore.Authentication.JwtBearer
open Shared
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Shared4.HttpTypes
open Config

let onlyLoggedIn = pipeline {
    requires_authentication (Giraffe.Auth.challenge JwtBearerDefaults.AuthenticationScheme)
}

let generateToken username claims = 
    let claims = [|
        Claim(JwtRegisteredClaimNames.Sub, username)
        Claim(JwtRegisteredClaimNames.Jti,Guid.NewGuid().ToString())

        yield! claims
    |]

    let expires = Nullable(DateTime.UtcNow.AddHours(8.0))
    let notBefore = Nullable(DateTime.UtcNow)
    let securityKey = SymmetricSecurityKey(Encoding.UTF8.GetBytes(Secret))
    let signingCredentials = SigningCredentials(key = securityKey, algorithm = SecurityAlgorithms.HmacSha256)

    let token = 
        JwtSecurityToken(
            issuer = Issuer,
            audience = Audience,
            claims = claims,
            expires = expires,
            notBefore = notBefore,
            signingCredentials = signingCredentials
        )

    let tokenResult = {
        Token = JwtSecurityTokenHandler().WriteToken(token)
    }

    tokenResult

type Saturn.Application.ApplicationBuilder with

    [<CustomOperation("my_own_jwt_auth")>]
    member __.UseMyMehAuth(state : ApplicationState,secret : string) =
        let middleware(app : IApplicationBuilder) = 
            app.UseAuthentication()
        
        let service (s : IServiceCollection) = 
            s.AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
                .AddJwtBearer(fun options ->
                    options.TokenValidationParameters <- TokenValidationParameters(
                        ValidateActor = true,
                        ValidateAudience = true,
                        ValidateLifetime = true,
                        ValidateIssuerSigningKey = true,
                        ValidIssuer = Issuer,
                        ValidAudience = Audience,
                        IssuerSigningKey = SymmetricSecurityKey(Encoding.UTF8.GetBytes(secret)))
                    ) |> ignore

            s

        { state with
            ServicesConfig = service :: state.ServicesConfig
            AppConfigs = middleware :: state.AppConfigs
            CookiesAlreadyAdded = true
        }



